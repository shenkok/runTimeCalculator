module Main where

import ImpSBV
import Imp
import ImpIO
import ImpVCGen
import ImpParser
import ImpProgram
import Data.SBV

------------------------------{MÉTODO PARA UNIR LOS PROCESOS}-----------------------------------------------------------------

run :: String -> IO ()
run input = case parseProgram "<interactive>" input of
  Left err  -> print err
  Right program -> completeRoutine (deepSimplifyProgram program) input rtZero

-- |Calcula la iteración de punto fijo de orden n de una transformada de un while determinista
fp :: String -> String -> String -> String -> Int ->IO ()
fp x b p runt n = case (parseRunTime "<interactive>" x, parseBExp "<interactive>" b, parseProgram "<interactive>" p, parseRunTime "<interactive>" runt) of
  (Right x', Right b',Right p',Right runt') -> print $ deepSimplifyRunTime $ fpWhile x' b' p' runt' n
  (_, _,  _, _) -> error "Ha ocurrido un error"

-- | Calcula la iteración de punto fijo de orden n de una transformada de un while probabilista
fpp :: String -> String -> String -> String -> Int ->IO ()
fpp x pb p runt n = case (parseRunTime "<interactive>" x, parsePBExp "<interactive>" pb, parseProgram "<interactive>" p, parseRunTime "<interactive>" runt) of
  (Right x', Right pb',Right p',Right runt') ->  print $ deepSimplifyRunTime $ fpPWhile x' pb' p' runt' n
  (_, _,  _, _) -> error "Ha ocurrido un error"


sol = sat $ do 
  x <- sRational "x"
  z <- sRational "z"
  constrain $ x  .> z + literal 0.75
  constrain $ x .<= 0

main =  run cTrunc