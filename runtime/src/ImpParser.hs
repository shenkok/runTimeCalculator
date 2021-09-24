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

--------------------------------------------------------------------------- {PARSER PARA EXPRESIONES ARITMÉTICAS}---------------------------------

-- AExp singular o monomio
-- monomio = n | x | n*x

-- AExp en Forma Normal Débil aefnd
-- aefnd = monomio | monomio + aefnd


rationalInteger :: Parser Rational
rationalInteger = do
  n <- integer
  return $ toRational n

rationalFractional :: Parser Rational
rationalFractional = do
    whitespace
    num <- integer
    void $ char '/'
    den <- integer
    whitespace
    return $ (toRational num)/ (toRational den)

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
           <|> try rationalAExp
           <|> try varAExp
           <|> try (parens aexp)
       table = [[binary "+" (:+:), binary "-" (-:) ]]

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

----------------------------------- { PARSER PARA RUNTIME} -------------------
-- RunTime singular o rts
-- rts = monomio | q*[bool]<>x | [bool]<>x | q*[bool]

-- RunTime en Forma Normal Débil o rtfnd
-- rtfnd = rts| rts ++ rtfnd

runtime :: Parser RunTime
runtime = buildExpressionParser table term
 where term =  try (RunTimeArit <$> aexp)
           <|> try (parens runtime)
       table = [ [ binary "++" (:++:), binary "--" (--:) ]]
        
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