
{-
 -  <DeclList> ::= { (<FunDecl> | <ValDecl>) }
 -
 -  <FunDecl> ::= 'fun' <Ident> <Ident> '=' <Exp> 
 -
 -  <ValDecl> ::= 'val' <Ident> '=' <Exp> 
 -
 -  <Expr> ::= <Comp> 
 -           | 'if' <Exp> 'then' <Exp> 'else' <Exp>
 -           | 'let' <DeclList> 'in' <Exp> 'end'
 -           | 'fn' <Ident> '=>' <Exp>
 -
 -  <Comp> ::= <Plus> { ('>' | '=' | '<') <Plus> }
 -
 -  <Plus> ::= <Mult> { ('+' | '-') <Mult> }
 -
 -  <Mult> ::= <App>  { ('*' | '/') <App> }
 -
 -  <App>  ::= <Fact> { <Fact> }
 -
 -  <Fact> ::= '(' <Exp> ')' 
 -          |  <Integer>     
 -          |  <Identifier>
 -}

{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Text.Parsec
import Text.Parsec.Prim (unexpected)
import Data.Either
 
-- Abstract Syntax Tree ----------------------------------------------------------------------------------

-- function/variable declaration
data Decl = Fun String String Exp -- fun f x = e;
          | Val String Exp        -- val x = e;

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
         | Let [Decl] Exp -- let val x = e0; fun f = e1; in e2 end
         | App Exp Exp    -- e1 e2
         | Const Integer  -- n

instance Show Decl where
  show (Fun f x e) = "fun " ++ f ++ " " ++ x ++ " = " ++ show e 
  show (Val x e) = "val " ++ x ++ " = " ++ show e 

instance Show Exp where
  show (Const x) = show x
  show (Plus t1 t2) = showBop t1 "+" t2
  show (Times t1 t2) = showBop t1 "*" t2
  show (Minus t1 t2) = showBop t1 "-" t2
  show (Div t1 t2) = showBop t1 "/" t2
  show (Lt t1 t2) = showBop t1 "<" t2
  show (Gt t1 t2) = showBop t1 ">" t2
  show (Eq t1 t2) = showBop t1 "=" t2
  show (If t0 t1 t2) = "if " ++ show t0 ++ " then " ++ show t1 ++ " else " ++ show t2
  show (Var s) = s
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Fn x t) = "(fn" ++ x ++ " => " ++ show t ++ ")"
  show (Let decls e) = "(let " ++ show decls ++ " in " ++ show e ++ " end)"

showBop t1 op t2 = "(" ++ show t1 ++ " " ++ op ++ " " ++ show t2 ++ ")"


-- Parser definitions ----------------------------------------------------------------------------------

type Parser = Parsec [Char] ()

-- Runner

-- print AST or error
run :: Show a => Parser a -> String -> IO()
run p input = putStrLn $ case parse p "" input of
       Left error -> show error
       Right x    -> show x

-- return Either AST or parse error 
runParser :: Parser a -> String -> Either ParseError a
runParser p input = parse p "" input 

-- Lexer

lexeme p = do { x <- p; skipMany space <?> ""; return x }  -- get rid of trailing spaces

symbol name = lexeme $ string name                         -- parse string and then remove spaces

ident :: Parser String                                     -- raw identifier
ident = (lexeme $ many1 letter) <?> "identifier"

integer :: Parser Integer                                  -- parse integer and return numeric value
integer = (read <$> (lexeme $ many1 digit)) <?> "integer"

keyword :: String -> Parser ()                             -- make keyword parser with backtrack
keyword name = (try $ symbol name) >> return ()

if_ = keyword "if"                                         -- keyword parsers
then_ = keyword "then"
else_ = keyword "else"
fn_ = keyword "fn"
fun_ = keyword "fun"
let_ = keyword "let"
in_ = keyword "in"
end_ = keyword "end"
val_ = keyword "val"

identifier :: Parser String                                -- identifier that won't collide with keywords
identifier = try $ do 
    name <- ident 
    if name `elem` reserved 
      then unexpected ("reserved word " ++ name) -- throw parse error 
      else return name 
  where 
    reserved = ["if", "then", "else", "fn", "fun", "let", "in", "end", "val"]

-- Parser for the (phrase) grammar

prog :: Parser [Decl]         
prog = skipMany space >> declList                              -- skip leading spaces

declList :: Parser [Decl]                                      -- parse a list of function or variable declarations
declList = many $ valDecl <|> funDecl

-- TODO

valDecl :: Parser Decl
valDecl = do
  val_
  id <- ident
  symbol "="
  exp <- expr
  return (Val id exp)

funDecl :: Parser Decl
funDecl = do
  fun_
  fnId <- ident
  id  <- ident
  symbol "="
  exp <- expr
  return (Fun fnId id exp)

constant :: Parser Exp
constant = Const <$> integer

var :: Parser Exp
var = Var <$> identifier

expr :: Parser Exp
expr = comp <|> condExpr <|> letExpr <|> lambdaFnExpr

condExpr :: Parser Exp
condExpr = do
  if_
  cond <- expr
  then_
  true <- expr
  else_
  false <- expr
  return (If cond true false)

letExpr :: Parser Exp
letExpr = do
  let_
  decls <- declList
  in_
  exp <- expr
  return (Let decls exp)

lambdaFnExpr :: Parser Exp
lambdaFnExpr = do
  fun_
  id <- ident
  keyword "=>"
  exp <- expr
  return (Fn id exp)

comp :: Parser Exp
comp = chainl1 plus op
  where op = Lt <$ symbol "<"
         <|> Gt <$ symbol "="
         <|> Eq <$ symbol ">"

plus :: Parser Exp
plus = chainl1 mult op
  where op = Plus <$ symbol "+" 
         <|> Minus <$ symbol "-"

mult :: Parser Exp
mult = chainl1 app op
 where op = Times <$ symbol "*" 
        <|> Div <$ symbol "/"

app :: Parser Exp
app = chainl1 factor op
  where op = App <$ symbol ""

parens :: Parser a -> Parser a
parens p = do
  symbol "("
  p' <- p
  symbol ")"
  return p'

factor :: Parser Exp
factor = parens expr <|> constant <|> var

-- Test code ----------------------------------------------------------------------------------

main :: IO ()
main = do
         let fact = "fun fact x = if x < 1 then 1 else x * fact (x-1)\n"
         let it = "val it = let val y = 10 in fact y end"
         
         let d = (fact ++ it) 

         run prog d