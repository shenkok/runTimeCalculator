{-# LANGUAGE RecordWildCards #-}
module ImpParser where

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Expr
import           Text.Parsec.Language (javaStyle)
import           Text.Parsec.String
import           Control.Monad (void, ap)
import qualified Text.Parsec.Token    as Token
import           Imp                  

-- pretending Haskell has a good module system…
Token.TokenParser {..} = Token.makeTokenParser javaStyle

binary name fun = Infix (fun <$ reservedOp name) AssocLeft

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

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
-- regularParse anyCha

{-
identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar
-}
rationalFractional :: Parser Rational
rationalFractional = do
    whitespace
    num <- many1 digit
    void $ char '/'
    den <- many1 digit
    whitespace
    return $ toRational $ (read num)/ (read den)

rationalInteger :: Parser Rational
rationalInteger = do
  n <- integer
  return $ toRational n

rational :: Parser Rational
rational = try rationalFractional <|> rationalInteger

rationalAExp :: Parser AExp
rationalAExp = do
  q <- rational
  return $ Lit q

varAExp :: Parser AExp
varAExp = do
  x <- identifier
  return $ Var x

aexp :: Parser AExp
aexp = buildExpressionParser table term 
 where term = try ((:*:) <$> (rational <* reservedOp "*") <*> varAExp)
           <|> Lit  <$> rational
           <|> Var <$> identifier
           <|> try (parens aexp)
       table = [[binary "+" (:+:) ]]

bexp :: Parser BExp
bexp = buildExpressionParser table term
  where term =  True' <$ reserved "true"
            <|> False' <$ reserved "false"
            <|> try ((:<=:) <$> (aexp <* reservedOp "<=") <*> aexp)
            <|> try ((:==:) <$> (aexp <* reservedOp "==") <*> aexp)
            <|> try (parens bexp)
        table = [ [ Prefix (Not <$ reservedOp "!") ]
                , [ binary "&&" (:&:), binary "||" (:|:) ]
                ]
{-
runtime :: Parser RunTime
runtime = buildExpressionParser table term
 where term =  RunTimeArit <$> aexp
           <|> Var <$> identifier
           <|> ((:**:) <$> (rational <* reservedOp "**") <*> rational)
           <|> parens aexp
       table = [ [ binary "+" (:+:)]]
-}        
{-
cmd :: Parser Cmd
cmd = foldl Seq Skip <$> (statement `sepBy1` symbol ";")
  where statement =  If <$> (reserved "if" *> bexp)
                        <*> braces cmd
                        <*> (reserved "else" *> braces cmd)
                 <|> While <$> (reserved "while" *> bexp)
                           <*> braces cmd
                 <|> Set <$> (name <* reservedOp ":=") <*> aexp

parseCmd file = parse (cmd <* eof) file
-}