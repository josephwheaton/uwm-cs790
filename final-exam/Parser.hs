{-
 -  <DeclList> ::= { (<FunDecl> | <ValDecl>) }
 -
 -  <FunDecl> ::= 'fun' <Ident> <Ident> '=' <Exp> 
 -
 -  <ValDecl> ::= 'val' <Ident> '=' <Exp> 
 -
 -  <Expr> ::= <Comp> 
 -           | 'if' <Expr> 'then' <Expr> 'else' <Expr>
 -           | 'let' <DeclList> 'in' <Expr> 'end'
 -           | 'fn' <Ident> '=>' <Expr>
 -
 -  <Comp> ::= <Plus> { ('>' | '=' | '<') <Plus> }
 -
 -  <Plus> ::= <Mult> { ('+' | '-') <Mult> }
 -
 -  <Mult> ::= <App>  { ('*' | '/') <App> }
 -
 -  <App>  ::= <Fact> { <Fact> }
 -
 -  <Fact> ::= '(' <Expr> ')' 
 -          |  <Integer>     
 -          |  <Identifier>
 -}

{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Text.Parsec
import Text.Parsec.Prim (unexpected)
import Data.Either
import AST
 
-- Parser definitions

type Parser = Parsec [Char] ()

-- Runner

-- print AST or error
run :: Show a => Parser a -> String -> IO()
run p input = putStrLn $ either show show $ parse p "" input 

-- return Either AST or parse error 
runP :: Parser a -> String -> Either ParseError a
runP p input = parse p "" input 

-- Lexer

lexeme p = do { x <- p; skipMany space <?> ""; return x }  -- get rid of trailing spaces

symbol name = lexeme $ string name                         -- parse string and then remove spaces

ident :: Parser String                                     -- raw identifier
ident = (lexeme $ do {x <- many1 letter; y <- string "'" <|> return ""; return $ x++y} ) <?> "identifier"

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

-- parser for phrase grammar

prog :: Parser DeclList         
prog = skipMany space >> declList                              -- skip leading spaces

declList :: Parser DeclList                                    -- parse a list of declarations
declList = Decls <$> (many $ valDecl <|> funDecl)

valDecl = do { val_; x <- ident; symbol "="; e <- expr; return $ Val x e } <?> "val decl" 
funDecl = do { fun_; f <- ident; x <- ident; symbol "="; e <- expr; return $ Fun f x e } <?> "fun decl"

expr = (ifExp <|> fnExp <|> letExp <|> comp) <?> "exp"
   
ifExp = do { if_; e0 <- expr; then_; e1 <- expr; else_; e2 <- expr; return $ If e0 e1 e2 } <?> "if exp"
fnExp = do { fn_; x <- ident; symbol "=>"; e <- expr; return $ Fn x e } <?> "fn exp"
letExp = do { let_; ds <- declList; in_; e <- expr; end_; return $ Let ds e } <?> "let exp"

comp = (plus `chainl1` compop) <?> "comparison"
plus = (mult `chainl1` addop) <?> "plus or minus"
mult = (app `chainl1` mulop) <?> "times or div"

app = ((fact `chainl1` (return App)) <?> "app exp")

fact =  parens expr 
    <|> (Const <$> integer) 
    <|> (Var <$> identifier)

parens p = do { symbol "("; x <- p; symbol ")"; return x } <?> "parens exp"

compop = (symbol ">" >> return Gt) <|> (symbol "<" >> return Lt) <|> (symbol "=" >> return Eq)
addop = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)
mulop = (symbol "*" >> return Times) <|> (symbol "/" >> return Div)
