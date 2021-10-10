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
--------------------------------------------------------------------------- {PARSER PARA EXPRESIONES ARITMÉTICAS}---------------------------------
-- | Parser que permite encerrar entre 0 o 1 parentesis una expresión
parens01 :: Parser a -> Parser a
parens01 parser = try parser <|> parens parser

-- AExp singular o monomio
-- monomio = n | x | n*x

-- AExp en Forma Normal Débil aefnd
-- aefnd = monomio | monomio + aefnd

-- | Parser para escribir a los racionales como enteros
rationalInteger :: Parser Rational
rationalInteger = do
  n <- integer
  return $ toRational n

-- | Parser para escribir a los racionales de la forma Int/Int
rationalFractional :: Parser Rational
rationalFractional = do
    whitespace
    num <- integer
    void $ char '/'
    den <- integer
    whitespace
    return $ (toRational num)/ (toRational den)


-- | Parser para ecribir racionales
rational :: Parser Rational
rational = try rationalFractional <|> rationalInteger

-- | Parser para escribir los literales de AExp
litAExp :: Parser AExp
litAExp = do
  q <- rational
  return $ Lit q

-- | Parser para escibir las variables de AExp
varAExp :: Parser AExp
varAExp = do
  x <- identifier
  return $ Var x

-- | Parser para los casos base de AExp
aexpBase :: Parser AExp
aexpBase = try litAExp <|> varAExp

-- | Parser para AExp
aexp :: Parser AExp
aexp = buildExpressionParser table term 
 where term = try ((:*:) <$> (rational <* reservedOp "*") <*> aexpBase)
           <|> try ((:*:) <$> (rational <* reservedOp "*") <*> parens aexp)
           <|> try aexpBase
           <|> try (parens aexp)
       table = [[binary "+" (:+:), binary "-" (-:) ]]


-- | Parser para BExp
bexp :: Parser BExp
bexp = buildExpressionParser table term
  where term =  True' <$ reserved "true"
            <|> False' <$ reserved "false"
            <|> try ((:<=:) <$> (aexp <* reservedOp "<=") <*> aexp)
            <|> try ((:==:) <$> (aexp <* reservedOp "==") <*> aexp)
            <|> try ((>=:)  <$> (aexp <* reservedOp ">=") <*> aexp)
            <|> try ((>:)   <$> (aexp <* reservedOp ">")  <*> aexp)
            <|> try ((<:)   <$> (aexp <* reservedOp "<")  <*> aexp)
            <|> try ((/=:)  <$> (aexp <* reservedOp "!=") <*> aexp)            
            <|> try (parens bexp)
        table = [ [ Prefix (Not <$ reservedOp "!") ]
                , [ binary "&&" (:&:), binary "||" (:|:) ]
                ]

----------------------------------- { PARSER PARA RUNTIME} -------------------
-- RunTime singular o rts
-- rts = monomio | q*[bool]<>x | [bool]<>x | q*[bool]

-- RunTime en Forma Normal Débil o rtfnd
-- rtfnd = rts| rts ++ rtfnd

-- | Parser para RunTimes Aritméticos
aritRunTime :: Parser RunTime
aritRunTime = do
  arit <- aexp
  return $ RunTimeArit arit

-- | Parser para indicatrices 
indicator :: Parser RunTime
indicator = do 
  b <- brackets bexp
  return $ toIndicator b

indArit :: Parser RunTime
indArit =((<>:) <$> (indicator <* reservedOp "<>") <*> parens01 aexp)

runtimeBase :: Parser RunTime
runtimeBase = try indArit <|> indicator <|> aritRunTime

runtime :: Parser RunTime
runtime = buildExpressionParser table term
 where term =  try ((:**:) <$> (rational <* reservedOp "**") <*> runtimeBase)
           <|> try ((:**:) <$> (rational <* reservedOp "**") <*> parens runtime)
           <|> try ((:<>:) <$> (brackets bexp <* reservedOp "<>") <*> runtimeBase)
           <|> try ((:<>:) <$> (brackets bexp <* reservedOp "<>") <*>  parens runtime)
           <|> try runtimeBase
           <|> try (parens runtime)
       table = [ [ binary "++" (:++:), binary "--" (--:) ]]

-- | Parser para expresiones booleanas probabilistas 
pbexp :: Parser PBExp
pbexp = do 
  q <- angles rational
  return $ Ber q

---------------------------------------------------{DISTRIBUCIONES CONOCIDAS} ------------------------------
dirac :: Parser PAExp
dirac = do
  arit <- angles aexp
  return $ Imp.dirac arit

coin :: Parser PAExp
coin = Imp.coin <$> (reserved "coin_flip" *> parens rational)

uniform :: Parser PAExp
uniform = do
  void $ string "uniform"
  whitespace
  void $ char '('
  whitespace
  q <- rational
  whitespace
  void $ comma
  whitespace
  p <- rational
  whitespace
  void $ char ')'
  return $ Imp.uniform q p

uniform1 :: Parser PAExp
uniform1 = Imp.uniform1 <$> (reserved "uniform" *> parens rational)

-- | Distribuciones discretas
discrete :: Parser PAExp
discrete = try ImpParser.dirac <|> try ImpParser.coin 
         <|> try ImpParser.uniform <|> try ImpParser.uniform1

-- | Parser para expresiones aritméticas probabilistas
paexp :: Parser PAExp
paexp = buildExpressionParser table term
 where term =  try ((*~:) <$> (rational <* reservedOp "*") <*> angles aexp)
          <|> try discrete
          <|> try (parens paexp)
       table = [[ binary "+" (++) ]]

program :: Parser Program
program = foldl Seq Imp.Empty  <$> (statement `sepBy1` symbol ";")
  where statement = If <$> try (reserved "if" *> parens bexp)
                        <*> braces program
                        <*> (reserved "else" *> braces program)
                 <|> it <$> try (reserved "it" *> parens bexp)
                     <*> braces program       
                 <|> PIf <$> try (reserved "pif" *> parens pbexp)
                      <*> braces program
                      <*> (reserved "pelse" *> braces program)
                 <|> pit <$> try (reserved "pit" *> parens pbexp)
                        <*> braces program   
                 <|> flipw While <$> try (reserved "while" *> parens bexp)
                          <*> braces (reserved "inv" *> reserved "=" *> runtime)
                          <*> braces program   
                 <|> flipw PWhile <$> try (reserved "pwhile" *> parens pbexp) 
                          <*> braces (reserved "pinv" *> reserved "=" *> runtime)
                          <*> braces program
                 <|> Set  <$> try (identifier <* reservedOp ":=") <*> aexp
                 <|> PSet <$> try (identifier <* reservedOp ":~") <*> paexp
                 <|> flip for  <$> try (reserved "for" *> parens integer) <*> braces program
                 <|> Skip <$ reserved "skip"
                 <|> Imp.Empty <$ reserved "empty"

------------------------------------------------{ PARSERS PARA LAS DIFERENTES ESTRUCTURAS}--------------------------
parseStruct :: Parser a -> SourceName -> String -> Either ParseError a
parseStruct p = parse (p <* eof)

parseAExp :: SourceName -> String -> Either ParseError AExp
parseAExp = parseStruct aexp

parseBExp :: SourceName -> String -> Either ParseError BExp
parseBExp = parseStruct bexp

parseRunTime :: SourceName -> String -> Either ParseError RunTime
parseRunTime = parseStruct runtime

parsePBExp :: SourceName -> String -> Either ParseError PBExp
parsePBExp = parseStruct pbexp

parsePAExp :: SourceName -> String -> Either ParseError PAExp
parsePAExp = parseStruct paexp

parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram str = parse (program <* eof) str

parseProgram' ::  SourceName -> String -> Either ParseError Program
parseProgram' = parseStruct program