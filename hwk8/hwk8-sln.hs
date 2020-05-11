prog :: Parser [Decl]
prog = skipMany space >> declList -- skip leading spaces

declList:: Parser [Decl] -- parse a list of declarations
declList = many $ valDecl <|> funDecl

valDecl = do { val_; x <- ident; symbol "="; e <- expr; return $ Val x e } <?> "val decl"
funDecl = do { fun_; f <- ident; x <- ident; symbol "="; e <- expr; return $ Fun f x e } <?> "fun decl"

expr = (ifExp <|> fnExp <|> letExp <|> comp) <?> "exp"

ifExp = do { if_; e0 <- expr; then_; e1 <- expr; else_; e2 <- expr; return $ If e0 e1 e2 } <?> "if exp"
fnExp = do { fn_; x <- ident; symbol "=>"; e <- expr; return $ Fn x e } <?> "fn exp"
letExp = do { let_; ds <- declList; in_; e <- expr; end_; return $ Let ds e } <?> "let exp"

comp = (plus `chainl1` compop) <?> "comparison"
plus = (mult `chainl1` addop) <?> "plus or minus"
mult = (app `chainl1` mulop) <?> "times or div"

app = (fact `chainl1` (return App)) <?> "app exp"

fact = parens expr <|> (Const <$> integer)  <|> (Var <$> identifier)

parens p = do { symbol "("; x <- p; symbol ")"; return x } <?> "parens exp"

compop = (symbol ">" >> return Gt) <|> (symbol "<" >> return Lt) <|> (symbol "=" >> return Eq)
addop = (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)
mulop = (symbol "*" >> return Times) <|> (symbol "/" >> return Div)