module ParseZ3 where

import Text.Trifecta
import Control.Applicative
import Z3Type

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sampleVars1 = "x y z\n"
sampleVars2 = "x y z;"

test = parseTest z3Expr

{-
z3parser :: Parser Program
z3parser = do
    vs <- z3vars
    q <- symbol "{" *> next z3Expr <* symbol "}"
    p <- convert2Cmd <$> z3Expr
    r <- symbol "{" *> next z3Expr <* symbol "}"
    return Program{vars = vs, pre = q, cmd = p, post = r}

convert2Cmd :: Expr -> Cmd
convert2Cmd exp = 
-}
z3vars :: Parser [Var]
z3vars = do
    symbol "vars" >> varnames where
        varnames = (newline *> return [])
            <|> (:) <$> (try arrayvar <|> numvar) <*> varnames where
                numvar = NumVar <$> (spaces *> many alphaNum) 
                arrayvar = ArrayVar <$> (spaces *> many alphaNum <* symbol "[]")

term :: Parser Expr
term = 
    (ZTrue <$ symbol "true")
    <|> (ZFalse <$ symbol "false")
    <|> (symbol "(" *> next z3Expr <* symbol ")")
    <|> z3digit
    <|> z3varriance

z3cmd :: Parser Cmd
z3cmd =
    (Skip <$ symbol "skip")
    <|> (symbol "if" >> (IF <$> next z3Expr <*> (symbol "then" >> next z3cmd) <*> (symbol "else" >> next z3cmd)))

z3Expr :: Parser Expr
z3Expr = 
    (symbol "select" >> Select <$> next z3Expr <*> next z3Expr)
    <|> (symbol "store" >> Store <$> next z3Expr <*> next z3Expr <*> next z3Expr)
    <|> (symbol "imp" >> Imp <$> next z3Expr <*> next z3Expr)
    <|> (symbol "forall" >> Forall <$> next (many alphaNum) <*> next z3Expr)
    <|> (symbol "exists" >> Exists <$> next (many alphaNum) <*> next z3Expr)
    <|> try (next (try term <|> try z3Expr) `z3logical` next (try z3Expr))
    <|> try (next (try term <|> try z3Expr) `z3opePlusMinus` next (try z3Expr))
    <|> try (next (try term <|> try z3Expr) `z3opeOthers` next (try z3Expr))
    <|> term

next parser = spaces *> parser <* spaces

z3digit =  ZInt <$> read <$> ((:) <$> digit <*> many digit)
z3varriance = ZVar <$> ((:) <$> alphaNum <*> many alphaNum)

z3opePlusMinus parser1 parser2 = try (f <$> parser1 <*> (spaces *> oneOf "+-" <* spaces )<*> parser2) where
    f expr1 op expr2 = case op of
        '+' -> Add expr1 expr2
        '-' -> Sub expr1 expr2

z3opeOthers :: Parser Expr -> Parser Expr -> Parser Expr
z3opeOthers parser1 parser2 = try (f <$> parser1 <*> (spaces *> oneOf "*/%" <* spaces )<*> parser2) where
    f expr1 op expr2 = case op of
        '*' -> Mul expr1 expr2
        '/' -> Div expr1 expr2
        '%' -> Mod expr1 expr2

z3logical parser1 parser2 =
    try (symbol "!" >> NOT <$> next z3Expr)
    <|> try (logicOpe "==")
    <|> try (logicOpe "and")
    <|> try (logicOpe "or")
    <|> try (logicOpe ">=")
    <|> try (logicOpe ">")
    <|> try (logicOpe "<=")
    <|> try (logicOpe "<") where
        logicOpe ope = case ope of
            "=="    ->  ZEq <$> (parser1 <* symbol ope) <*> parser2
            "and"   ->  AND <$> (parser1 <* symbol ope) <*> parser2 
            "or"    ->  OR <$> (parser1 <* symbol ope ) <*> parser2 
            ">="    ->  Ge <$> (parser1 <* symbol ope ) <*> parser2 
            ">"     ->  Gt <$> (parser1 <* symbol ope ) <*> parser2 
            "<="    ->  Le <$> (parser1 <* symbol ope ) <*> parser2 
            "<"     ->  Lt <$> (parser1 <* symbol ope ) <*> parser2  

preCondtion :: Parser (Maybe Expr)
preCondtion = return Nothing

postCondition :: Parser (Maybe Expr)
postCondition = return Nothing
    
genExpr :: Expr -> String
genExpr expr = "(assert (" ++ strExpr ++ ")" where
    strExpr = case expr of
        ZTrue -> "true"
        ZFalse -> "false"
        ZInt v -> show v
        ZVar name -> name
        Select expr1 expr2 -> "select " ++ genExpr expr1 ++ genExpr expr2
        Store expr1 expr2 expr3 -> "store " ++ genExpr expr1 ++ genExpr expr2 ++ genExpr expr3
        Add expr1 expr2 -> " + " ++ genExpr expr1 ++ genExpr expr2
        Sub expr1 expr2 -> " - " ++ genExpr expr1 ++ genExpr expr2
        Mul expr1 expr2 -> " * " ++ genExpr expr1 ++ genExpr expr2
        Div expr1 expr2 -> " div " ++ genExpr expr1 ++ genExpr expr2
        Mod expr1 expr2 -> "  mod " ++ genExpr expr1 ++ genExpr expr2
        ZEq expr1 expr2 -> "= " ++ genExpr expr1 ++ genExpr expr2
        Lt expr1 expr2 -> " < " ++ genExpr expr1 ++ genExpr expr2
        Le expr1 expr2 -> " <= " ++ genExpr expr1 ++ genExpr expr2
        Gt expr1 expr2 -> " > " ++ genExpr expr1 ++ genExpr expr2
        Ge expr1 expr2 -> " >= " ++ genExpr expr1 ++ genExpr expr2
        NOT expr1 -> " not " ++ genExpr expr1
        AND expr1 expr2 -> " and " ++ genExpr expr1 ++ genExpr expr2
        OR expr1 expr2 -> " or " ++ genExpr expr1 ++ genExpr expr2
        Imp expr1 expr2 -> " => " ++ genExpr expr1 ++ genExpr expr2
        Forall name expr1 -> "forall ((" ++ name ++ "Int))" ++ genExpr expr1
        Exists name expr1 -> "exists ((" ++ name ++ "Int))" ++ genExpr expr1

subst name expr1 expr2 = case expr2 of
    ZFalse -> ZFalse


--最弱事前条件を生成する
genWp acc cmd expr = case cmd of
    Skip -> (acc, expr)
    Assign name expr1 -> (acc, subst name expr1 expr)
    Update name expr1 expr2 -> (acc, subst name (Store (ZVar name) expr1 expr2) expr)
    IF expr1 cmd1 cmd2 -> (acc'', AND (Imp expr1 pre1) (Imp (NOT expr1) pre2)) where
        (acc', pre1) = genWp acc cmd1 expr
        (acc'', pre2) = genWp acc' cmd2 expr
    While expr1 expr2 cmd -> (_Q:_R:acc', expr2) where
        (acc', pre) = genWp acc cmd expr2
        _Q = Imp (AND expr1 expr2) pre
        _R = Imp (AND (NOT expr1) expr2) expr
    Seq cmd1 cmd2 -> genWp acc' cmd1 pre where
        (acc', pre) = genWp acc cmd2 expr

{-
runXML :: Parser [XML]
runXML = (:) <$> xml <*> (runXML <|> return [])

xml :: Parser XML
xml = do
    (tag,attrs) <- (,) <$> (symbol "<" *> many alphaNum) <*> attributes
    (mkXML tag attrs) <$> value <*> node tag

attributes :: Parser [(String,String)]
attributes = do
    spaces
    [] <$ symbol ">" <|> (:) <$> elem <*> attributes where
        elem = (,) <$> attr <*> attrValue
        attr = many alphaNum
        term = spaces *> symbol "=" <* spaces
        attrValue = term *> stringLiteral <* spaces

value :: Parser String
value = many (noneOf "<")

node :: String -> Parser (Maybe XML)
node tagName = Nothing <$ isEnd <|> Just <$> xml <* isEnd where
    isEnd = symbol $ "</" ++ tagName ++ ">"
main = do
    file <- getLine
    parseTest (runXML <* eof) =<< readFile file
-}
