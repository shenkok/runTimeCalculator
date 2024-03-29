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
import Control.Exception (bracket_)


{-
  MÓDULO QUE SE ENCARGA DE DEFINIR LOS PARSERS PARA LAS DIFERENTES ESTRUCTURAS
-}

-------------------------------{DEFINICIONES ÚTILES}-------------------------------------------------------------------------------------------
Token.TokenParser {..} = Token.makeTokenParser javaStyle

-- | Define la asociación por la izquierda
binary name fun = Infix (fun <$ reservedOp name) AssocLeft

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""
--------------------------------------------------------------------------- {PARSER PARA EXPRESIONES ARITMÉTICAS}---------------------------------
-- | Parser que permite encerrar entre 0 o 1 parentesis una expresión
parens01 :: Parser a -> Parser a
parens01 parser = try parser <|> parens parser


-- | Parser para escribir a los racionales como enteros
rationalInteger :: Parser Rational
rationalInteger = toRational <$> integer

-- | Parser para escribir a los racionales de la forma Int/Int
rationalFractional :: Parser Rational
rationalFractional = do
    whitespace
    num <- integer
    void $ char '/'
    den <- integer
    whitespace
    return $ toRational num/ toRational den



-- | Parser para ecribir racionales
rational :: Parser Rational
rational = try rationalFractional <|> rationalInteger

-- | Parser para escribir los literales de AExp
litAExp :: Parser AExp
litAExp = Lit <$> rational

-- | Parser para escibir las variables de AExp
varAExp :: Parser AExp
varAExp = Var <$> identifier

-- | Parser para los casos base de AExp
--     aexpBase -> identifier
--              | q/p
--              | n   
aexpBase :: Parser AExp
aexpBase = try litAExp <|> varAExp

-- | Parser para AExp
--  aexp -> aexpBase 
--        | rational * aexpBase
--        | rational * (aexp)
--        | aexp + aexp
--        | aexp - aexp azúcar sintáctica
aexp :: Parser AExp
aexp = buildExpressionParser table term
 where term = try ((:*:) <$> (rational <* reservedOp "*") <*> aexpBase)
           <|> try ((:*:) <$> (rational <* reservedOp "*") <*> parens aexp)
           <|> try aexpBase
           <|> try (parens aexp)
       table = [[binary "+" (:+:), binary "-" (-:) ]]


-- | Parser para BExp
-- bexp -> true, false
--       | aexp <= aexp
--       | aexp == aexp
--       | aexp >= aexp azúcar sintáctica 
--       | aexp >  aexp azúcar sintáctica
--       | aexp <  aexp azúcar sintáctica
--       | aexp != aexp azúcar sintáctica
--       | ! bexp
--       | bexp && bexp
--       | bexp || bexp

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

-- | Parser para RunTimes Aritméticos
aritRunTime :: Parser RunTime
aritRunTime = RunTimeArit <$> aexp

-- | Parser para indicatrices 
indicator :: Parser RunTime
indicator = toIndicator <$> brackets bexp

-- | Parser para una indicatriz por una expresión aritmética
indArit :: Parser RunTime
indArit = (<>:) <$> (indicator <* reservedOp "<>") <*> parens01 aexp

-- | Parser para runtimes bases
--  runtimeBase -> [bexp]
--               | aexp
--               | [bexp] <> aexp      
runtimeBase :: Parser RunTime
runtimeBase = try indArit <|> indicator <|> aritRunTime

-- | Parser para runtimes
-- runtime -> runtimeBase
--          | rational ** runtimeBase
--          | rational ** (runtime)
--          | rational <> runtimeBase 
--          | rational <> (runtime)
--          | runtime ++ runtime
--          | runtime -- runtime

runtime :: Parser RunTime
runtime = buildExpressionParser table term
 where term =  try ((:**:) <$> (rational <* reservedOp "**") <*> runtimeBase)
           <|> try ((:**:) <$> (rational <* reservedOp "**") <*> parens runtime)
           <|> try ((:<>:) <$> (brackets bexp <* reservedOp "<>") <*> runtimeBase)
           <|> try ((:<>:) <$> (brackets bexp <* reservedOp "<>") <*>  parens runtime)
           <|> try runtimeBase
           <|> try (parens runtime)
       table = [ [ binary "++" (:++:), binary "--" (--:) ]]

-- | Parser para expresiones booleanas probabilistas, distruciones bernoulli sobre true false
pbexp :: Parser PBExp
pbexp = Ber <$> angles rational

---------------------------------------------------{DISTRIBUCIONES CONOCIDAS} ------------------------------
-- | Parser para una distribución de dirac
dirac :: Parser PAExp
dirac = Imp.dirac <$> angles aexp

-- | Parser para una distribución bernoulli sobre 0 1
coin :: Parser PAExp
coin = Imp.coin <$> (reserved "coin_flip" *> parens rational)

-- | Parser para distribucipon uniforme sobre los enteros
uniform :: Parser PAExp
uniform = do
  void $ string "uniform"
  whitespace
  void $ char '('
  whitespace
  q <- rational
  whitespace
  void comma
  whitespace
  p <- rational
  whitespace
  void $ char ')'
  return $ Imp.uniform q p

-- | Parser para distribución uniforme sobre los enteros desde el 1
uniform1 :: Parser PAExp
uniform1 = Imp.uniform1 <$> (reserved "uniform" *> parens rational)

-- | Parser para Distribuciones discretas 
-- discrete -> <arit> 
--           | coin_flip(rational)
--           | uniform(rational, rational)
--           | uniform(rational)

discrete :: Parser PAExp
discrete = try ImpParser.dirac <|> try ImpParser.coin
         <|> try ImpParser.uniform <|> try ImpParser.uniform1

-- | Parser para expresiones aritméticas probabilistas
-- paexp -> discrete
--        | rational * <aexp>
--        | paexp + paexp
paexp :: Parser PAExp
paexp = buildExpressionParser table term
 where term =  try ((*~:) <$> (rational <* reservedOp "*") <*> angles aexp)
          <|> try discrete
          <|> try (parens paexp)
       table = [[ binary "+" (++) ]]

-- | Parser para programas
-- program -> skip
--          | empty
--          | indentifier := aexp
--          | indentifier :~ paexp
--          | program ; program
--          | if (bexp) {program} else {program}
--          | pif (<rational>) {program} pelse {program}
--          | it (bexp) {program}         Azúcar sintáctica
--          | pit (<rational>) {program}  Azúcar sintáctica
--          | while (bexp) {inv = runtime}{program}
--          | pwhile (<rational>) {pinv = runtime}{program}
--          | for (integer) {program}     Azúcar sintáctica
--

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

------------------------------------------------{MÉTODOS PARA USAR LOS PARSER ANTERIORES}--------------------------
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
parseProgram = parse (program <* eof)

parseProgram' ::  SourceName -> String -> Either ParseError Program
parseProgram' = parseStruct program

