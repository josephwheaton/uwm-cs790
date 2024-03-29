{-# LANGUAGE FlexibleContexts #-} 

module Eval where

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Writer
import Control.Monad.Except

import Data.List
import Debug.Trace

import AST

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
evalD (Decls []) = do
  context <- ask
  return context
evalD (Decls (x:xs)) =
  let evalD' (Val ident e) = do
        -- traceM("Running evalD Val: " ++ ident)
        v <- eval e
        return (ident, v)
      evalD' (Fun ident x e) = do
        -- traceM("Running evalD Fun: " ++ ident)
        localContext <- ask
        return (ident, FVal (Just ident, x, e) localContext)
  in do
    context <- ask
    nextDecl <- evalD' x
    let context' = nextDecl : context
      in local (const context') $ evalD (Decls xs)

eval :: Exp -> Eval Val
eval (Lt e1 e2) = do
  -- traceM("Running eval Lt")
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ BoolVal $ i1 < i2
        _ -> throwError $ NotABool v2
    _ -> throwError $ NotABool v1

eval (Gt e1 e2) = do
  -- traceM("Running eval Gt")
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ BoolVal $ i1 > i2
        _ -> throwError $ NotABool v2
    _ -> throwError $ NotABool v1

eval (Eq e1 e2) = do
  -- traceM("Running eval Eq")
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ BoolVal $ i1 == i2
        _ -> throwError $ NotABool v2
    _ -> throwError $ NotABool v1

eval (Plus e1 e2) = do
  -- traceM("Running eval Plus")
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ IntVal $ i1 + i2
        _ -> throwError $ NotAnInt v2
    _ -> throwError $ NotAnInt v1

eval (Minus e1 e2) = do
  -- traceM("Running eval Minus")
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ IntVal $ i1 - i2
        _ -> throwError $ NotAnInt v2
    _ -> throwError $ NotAnInt v1

eval (Times e1 e2) = do
  -- traceM("Running eval Times")
  v1 <- eval e1
  case v1 of
    IntVal i1 -> do
      v2 <- eval e2
      case v2 of
        IntVal i2 -> return $ IntVal $ i1 * i2
        _ -> throwError $ NotAnInt v2
    _ -> throwError $ NotAnInt v1

eval (Div e1 e2) = do
  -- traceM("Running eval Div")
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
  -- traceM("Running eval Var: " ++ s)
  context <- ask
  -- traceM("Here is context: " ++ show context)
  let var = find (\x -> fst x == s) context
  case var of
    Nothing -> throwError $ VariableNotFound s
    Just (_, val) -> return val

eval (If e0 e1 e2) = do
  -- traceM("Running eval If")
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
  -- traceM("Running eval Fn: " ++ show s)
  localContext <- ask
  return $ FVal (Nothing, s, e) localContext

eval (Let dl e) = do
  -- traceM("Running eval Let")
  context <- ask
  additionalContext <- evalD dl
  let context' = additionalContext <> context
    in local (const context') $ eval e

eval (App f x) = do
  -- traceM("Running eval App")
  f_v <- eval f
  x_v <- eval x
  context <- ask
  case f_v of
    FVal (ident, x_id, fn) localContext ->
      case ident of
        -- ? lambda
        Nothing -> 
          let context' = [(x_id, x_v)] <> localContext
            in 
              -- trace("Show applying Fn with param " ++ x_id ++ " and value " ++ show x_v)
              local (const context') $ eval fn
        -- ? fun
        Just identifier -> 
          let context' = [(x_id, x_v)] <> context
            in
              -- trace("Show applying Fun " ++ identifier ++ " with param " ++ x_id ++ " and value " ++ show x_v)
              local (const context') $ eval fn
    _ -> throwError $ NotAFun f_v

eval (Const i) = do
  -- traceM("Running eval Const: " ++ show i)
  return $ IntVal i

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