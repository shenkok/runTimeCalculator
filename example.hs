module Math where

import Text.Parsec
import Text.Parsec.String

data Expr
  = Num Integer
  | Op Op Expr Expr
  deriving Show

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Less
  | LessE
  | Greater
  | GreaterE
  | Equals
  deriving Show

eval :: String -> Integer
eval = eval' . parseExpr

eval' :: Expr -> Integer
eval' (Num x)           = x
eval' (Op Add x y)      = eval' x + eval' y
eval' (Op Sub x y)      = eval' x - eval' y
eval' (Op Mul x y)      = eval' x * eval' y
eval' (Op Div x y)      = eval' x `div` eval' y
eval' (Op Mod x y)      = eval' x `mod` eval' y
eval' (Op Less x y)     = bool $ eval' x <  eval' y
eval' (Op LessE x y)    = bool $ eval' x <= eval' y
eval' (Op Greater x y)  = bool $ eval' x >  eval' y
eval' (Op GreaterE x y) = bool $ eval' x >= eval' y
eval' (Op Equals x y)   = bool $ eval' x == eval' y

bool :: Bool -> Integer
bool True  = 1
bool False = 0

-- Parser

parseExpr :: String -> Expr
parseExpr s =
  case parse (whitespace *> compareExpr <* eof) "" s of
    Left e  -> error $ show e
    Right x -> x

compareExpr :: Parser Expr
compareExpr = compare'
          <|> expression

compare' :: Parser Expr
compare' = try $ do
  x  <- expression
  op <- choice [ Less     <$ char '<'
               , LessE    <$ try (string "<=")
               , Greater  <$ char '>'
               , GreaterE <$ try (string ">=")
               , Equals   <$ try (string "==")
               ] <* whitespace
  y  <- expression
  return $ Op op x y

expression :: Parser Expr
expression = addExpr
         <|> mulExpr
         <|> number
         <|> parens expression

addExpr :: Parser Expr
addExpr = try $ expression' `chainl1` operator
  where expression' = mulExpr
                  <|> number
                  <|> parens expression

        operator = choice
          [ Op Add <$ char '+'
          , Op Sub <$ char '-'
          ] <* whitespace

mulExpr :: Parser Expr
mulExpr = try $ expression' `chainl1` operator
  where expression' = number
                  <|> parens expression

        operator = choice
          [ Op Mul <$ char '*'
          , Op Div <$ char '/'
          , Op Mod <$ char '%'
          ] <* whitespace

number :: Parser Expr
number = do
  sign   <- option ' ' (char '-')
  digits <- many1 digit
  whitespace
  return $ Num $ read (sign:digits)

parens :: Parser a -> Parser a
parens p = between open close p
  where open  = char '(' <* whitespace
        close = char ')' <* whitespace

whitespace :: Parser ()
whitespace = skipMany space