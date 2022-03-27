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

-- Ejemplo iteración número 3, para el programa while(x==0){x:x-1}, con respecto al tiempo de ejcución 0 y empezando desde 0
exampleFp :: IO ()
exampleFp = fp "0" "x==0" "x:=x-1" "0" 3

-- | Calcula la iteración de punto fijo de orden n de una transformada de un while probabilista
fpp :: String -> String -> String -> String -> Int ->IO ()
fpp x pb p runt n = case (parseRunTime "<interactive>" x, parsePBExp "<interactive>" pb, parseProgram "<interactive>" p, parseRunTime "<interactive>" runt) of
  (Right x', Right pb',Right p',Right runt') ->  print $ deepSimplifyRunTime $ fpPWhile x' pb' p' runt' n
  (_, _,  _, _) -> error "Ha ocurrido un error"

-- Ejemplo ieteración número 5, para el programa pwhile (<1/2>) {skip} con respecto al tiempo de ejecución 3 y empezando desde 0

exampleFpp :: IO ()
exampleFpp = fpp "0" "<1/2>" "skip" "3" 5

main =  run cTrunc