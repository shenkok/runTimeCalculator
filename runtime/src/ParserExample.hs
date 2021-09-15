{-# LANGUAGE RecordWildCards #-}
module ParserExample where
import Imp
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Expr
import           Text.Parsec.Language (javaStyle)
import           Text.Parsec.String
import           Control.Monad (void, ap)
import qualified Control.Applicative as A

import           Data.Char (isLetter, isDigit)
import qualified Text.Parsec.Token    as Token

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
   where leftOver = manyTill anyToken eof

parseWithWSEof :: Parser a -> String -> Either ParseError a
parseWithWSEof p = parseWithEof (whiteSpace *> p)
   where whiteSpace = void $ many $ oneOf " \n\t"

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
        whitespace
        p
-- regularParse anyChar "aaaa

-- regularParse (many1 digit) "122" uno o mas

-- regularParse (many digit) "122" 0  o más
-- regularParse (satisfy (=='a')) "a"
--regularParse  (many satisfy (=='a')) "a"
-- regularParse (oneOf "abc") "c"
-- regularParse (string "one") "two"

num :: Parser Integer
num = do
    n <- many1 digit -- recordar que retorna un string
    return (read n)

{-
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x
-}
rational :: Parser Rational
rational = do
    whitespace
    num <- many1 digit
    void $ char '/'
    den <- many1 digit
    whitespace
    return $ toRational $ (read num)/ (read den)


data SimpleExpr = Num Integer
                | V String
                | Add SimpleExpr SimpleExpr
                | Parens SimpleExpr
                  deriving (Eq,Show)

{-
parensL :: Parser Parentheses
parensL = do
    void $ lexeme $ char '('
    e <- lexeme $ many1 digit
    void $ lexeme $ char ')'
    return (Parentheses (read e))-}
numE :: Parser SimpleExpr
numE = do
    n <- lexeme $ many1 digit
    return $ Num $ read n

varE' :: Parser SimpleExpr
varE' = do
    fc <- firstChar
    rest <- lexeme $ many nonFirstChar
    return $ V (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parensE' :: Parser SimpleExpr
parensE' = do
    void $ lexeme $ char '('
    e <- numE
    void $ lexeme $ char ')'
    return $ Parens e

addE :: Parser SimpleExpr
addE = do
    e0 <- numE
    void $ lexeme $ char '+'
    e1 <- numE
    return $ Add e0 e1

simpleExpr :: Parser SimpleExpr
simpleExpr = try addE <|> numE <|> varE' <|> parensE'

parensE4 :: Parser SimpleExpr
parensE4 = do
    void $ lexeme $ char '('
    e <- simpleExpr4
    void $ lexeme $ char ')'
    return $ Parens e

simpleExpr4 :: Parser SimpleExpr
simpleExpr4 = numE <|> varE' <|> parensE4

-- parseWithWhitespace simpleExpr4 "((a))"

parensEN :: Parser SimpleExpr -> Parser SimpleExpr
parensEN simpleExprImpl = do
    void $ lexeme $ char '('
    e <- simpleExprImpl
    void $ lexeme $ char ')'
    return $ Parens e

---------------------------------------------------------- Fin e estos ejemplos----------------------------------------
symbol :: Char -> Parser Char
symbol c = lexeme $ char c

lexeme :: Parser a -> Parser a
lexeme p = p A.<* whitespace

whitespace :: Parser ()
whitespace = void $ oneOf " \n\t"

