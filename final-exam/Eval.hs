{-# LANGUAGE FlexibleContexts #-} 

module Eval where

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Writer
import Control.Monad.Except

import Data.List

-- Abstract Syntax Tree

-- function/variable declaration
data Decl = Fun String String Exp  -- fun f x = e;
          | Val String Exp         -- val x = e;

newtype DeclList = Decls [Decl]

-- expressions
data Exp = Lt Exp Exp     -- e1 < e2
         | Gt Exp Exp     -- e1 > e2
         | Eq Exp Exp     -- e1 = e2
         | Plus Exp Exp   -- e1 + e2
         | Minus Exp Exp  -- e1 - e2
         | Times Exp Exp  -- e1 * e2
         | Div Exp Exp    -- e1 div e2
         | Var String     -- x
         | If Exp Exp Exp -- if e0 then e1 else e2
         | Fn String Exp  -- fn x => e
         | Let DeclList Exp -- let val x = e0; fun f = e1; in e2 end
         | App Exp Exp    -- e1 e2
         | Const Integer  -- n

instance Show DeclList where
  show (Decls decls) = unlines $ map show decls

instance Show Decl where
  show (Fun f x e) = "fun " ++ f ++ " " ++ x ++ " = " ++ show e
  show (Val x e) = "val " ++ x ++ " = " ++ show e

instance Show Exp where
  show (Const x) = show x
  show (Plus e1 e2) = show_op e1 "+" e2
  show (Times e1 e2) = show_op e1 "*" e2
  show (Minus e1 e2) = show_op e1 "-" e2
  show (Div e1 e2) = show_op e1 "/" e2
  show (Lt e1 e2) = show_op e1 "<" e2
  show (Gt e1 e2) = show_op e1 ">" e2
  show (Eq e1 e2) = show_op e1 "=" e2
  show (If e0 e1 e2) = "if " ++ show e0 ++ " then " ++ show e1 ++ " else " ++ show e2
  show (Var s) = s
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Fn x e) = "(fn " ++ x ++ " => " ++ show e ++ ")"
  show (Let (Decls decls) e) = "(let " ++ show decls ++ " in " ++ show e ++ " end)"

show_op e1 op e2 = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"

type Context = [(String, Val)]

data Val = IntVal Integer
         | BoolVal Bool
-- ? Nothing would represent an anonymous function expression
         | FVal (Maybe String, String, Exp) Context

instance Show Val where
  show (IntVal x) = show x
  show (BoolVal b) = show b
  show (FVal (Just f, x, e) _) =  "fn" -- "fun " ++ f ++ "..."
  show (FVal (Nothing, x, e) _) =  "(fn " ++ x ++ " => ...)"

data EvalError = VariableNotFound String
               | NotAnInt Val
               | NotABool Val
               | DivByZero
               | NotAFun Val deriving (Show)

-- reader+either monad for interpreter functions
--   'reader' for remembering val/fun declarations in contexts
--   'either' for throwing evaluation errors
type Eval = ReaderT Context (Either EvalError)

-- evaluate a list of declarations
evalD :: DeclList -> Eval Context
evalD (Decls list) =
  let evalD' (Val ident e) = do
        v <- eval e
        return (ident, v)
      evalD' (Fun ident x e) = do
        localContext <- ask
        return (ident, FVal (Just ident, x, e) localContext)
  in mapM evalD' list

eval :: Exp -> Eval Val
eval (Lt e1 e2) = do
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ BoolVal $ i1 < i2
        _ -> throwError $ NotABool v2
    _ -> throwError $ NotABool v1

eval (Gt e1 e2) = do
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ BoolVal $ i1 > i2
        _ -> throwError $ NotABool v2
    _ -> throwError $ NotABool v1

eval (Eq e1 e2) = do
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ BoolVal $ i1 == i2
        _ -> throwError $ NotABool v2
    _ -> throwError $ NotABool v1

eval (Plus e1 e2) = do
                          v1 <- eval e1
                          case v1 of
                            IntVal i1 -> do
                              v2 <- eval e2
                              case v2 of
                                IntVal i2 -> return $ IntVal $ i1 + i2
                                _ -> throwError $ NotAnInt v2
                            _ -> throwError $ NotAnInt v1

eval (Minus e1 e2) = do
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ IntVal $ i1 - i2
        _ -> throwError $ NotAnInt v2
    _ -> throwError $ NotAnInt v1

eval (Times e1 e2) = do
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ IntVal $ i1 * i2
        _ -> throwError $ NotAnInt v2
    _ -> throwError $ NotAnInt v1

eval (Div e1 e2) = do
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> 
          if i2 > 0 
          then return $ IntVal $ i1 `div` i2
          else throwError DivByZero
        _ -> throwError $ NotAnInt v2
    _ -> throwError $ NotAnInt v1

eval (Var s) = do
  context <- ask
  let var = find (\x -> fst x == s) context
  case var of
    Nothing -> throwError $ VariableNotFound s
    Just (_, val) -> return val

eval (If e0 e1 e2) = do
  v0 <- eval e0;
  case v0 of
    BoolVal p ->
      if p 
      then do
        v1 <- eval e1
        return v1
      else do
        v2 <- eval e2
        return v2
    _ ->  throwError $ NotABool v0

eval (Fn s e) = do
  localContext <- ask
  return $ FVal (Nothing, s, e) localContext

eval (Let dl e) = do
  context <- ask
  additionalContext <- evalD dl
  let context' = additionalContext <> context
    in local (const context') $ eval e

eval (App f x) = do
  f_v <- eval f
  x_v <- eval x
  case f_v of
    FVal (ident, x_id, fn) localContext ->
      case ident of
        -- ? lambda
        Nothing -> 
          let context' = [(x_id, x_v)] <> localContext
            in local (const context') $ eval fn
        -- ? fun
        Just _ -> eval fn
    _ -> throwError $ NotAFun f_v

eval (Const i) = do return $ IntVal i


-- run a list of declarations and print the resulting context
runD :: DeclList -> String
runD d = y 
  where Right y = runReaderT x []
        x = do  
              a <- evalD d
              return $ "answers:\n" ++ toString a ++ "\n"
            `catchError` (\e -> return $ show e ++ "\n") 
        toString a = unlines $ map (\(x,v)-> "\t val " ++ x ++ " = " ++ show v) $ reverse a

-- run an expression and print the results
runE :: Exp -> String
runE e = y 
  where Right y = runReaderT x []
        x = do  
              a <- eval e
              return $ "answers: " ++ show a ++ "\n"
            `catchError` (\e -> return $ show e ++ "\n")