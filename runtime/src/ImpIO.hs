module ImpIO where

import Data.SBV
import ImpToSBV
import Imp
import ImpToSBVInput
{-
    MODULO QUE SE ENCARGA SE IMPRIMIR LAS DIFERENTES VARIABLES DEL PROBLEMA ESTUDIADO
-}
uncurry3                        :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a, b, c)           = f a b c

uncurry4                        :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(a, b, c, d)        = f a b c d

boolIO :: Bool -> IO Bool
boolIO = pure

{-
ioBool :: IO Bool -> Bool
ioBool io_b | io_b == (boolIO True) = True
            | otherwise = False
-}
array :: [IO Bool]
array = map boolIO [True, True, False, False, True]

ioAnd :: IO Bool -> IO Bool -> IO Bool
ioAnd b_1 b_2 = (&&) <$> b_1 <*> b_2

newLine :: String
newLine = "\n"

space :: String
space = "  "
index :: Int -> String
index n = "[" ++ (show n) ++ "]"

index2 :: Int -> Int -> String
index2 n m = "[" ++ (show n) ++ ", " ++ (show m) ++ "]"

showRestrictions :: [RRunTime] -> Int -> String
showRestrictions [] _     = newLine
showRestrictions (x:xs) n = (index n) ++ space ++ (show x) ++ (showRestrictions xs (n-1))  

-- | Muestra la transformada calculada y las restricciones que se generaron
showTransform :: RunTime -> [RRunTime] -> Int ->IO()
showTransform ert restrictions n = if n > 0
                                       then do 
                                          putStrLn "Tiempo de ejecución calculado:"
                                          putStrLn $ show ert
                                          putStr newLine
                                          putStrLn "Obligaciones de prueba asociadas:"
                                          putStrLn $ showRestrictions restrictions n
                                        else
                                          putStrLn "No hay obligaciones de prueba asociadas"


-- | Muestra las distintos componentes de un problema 
--   en concreto.
{- Ejemplo de problema
        a <- sFloat "a"
        b <- sFloat "b"
        c <- sFloat "c"
        constrain $ a + 10.0 .< 19.0 + b
        constrain $ a + b + c.<= 10 
-}
-- Advierte si el problema no es satisfacible entregando un contraejemplo
showSolverInput :: SolverInput -> Int -> Int ->IO()
showSolverInput (contexto, rest, vars) n m = do
  let model = makeSBVModel (contexto, rest, vars)
  let lenb = not (null vars)
  b <- isSatisfiable model
  putStr newLine
  putStrLn $ "Sub-problema " ++ (index2 n m) 
  putStr newLine 
  putStrLn $ show contexto ++ " ----> " ++ show rest
  putStr newLine
  if b
      then do   values <- sat model
                putStrLn "El Sub-problema no es válido"
                putStr newLine
                if lenb
                  then do
                      putStrLn "Un contraejemplo es"
                      print values
                  else
                    putStrLn ""
      else putStrLn "El Sub-problema es válido"
  putStrLn $ concat (replicate 100 "-")

-- | Muestra un arreglo de problemas, n es un entero de denota la restricion n
showSolverInputs ::RRunTime -> Int -> IO()
showSolverInputs runtr n = do
  putStrLn $ concat (replicate 100 "*")
  putStr newLine
  putStrLn $ "Para la obligación de prueba " ++ index n
  putStrLn $ (show runtr)
  putStr newLine 
  putStrLn $ "Hay un total de " ++ show m ++ " sub-problemas diferentes." 
  putStrLn $ concat (replicate 100 "-")
  mapM_ (uncurry3 showSolverInput ) $ zip3  inputs (repeat n) [1..m]
  putStrLn $ concat (replicate 100 "*")
  where
    inputs = restrictionsToSolver runtr
    m = length inputs


-- | Muestra una rutina completa
-- Parte con un programa y un runtime
-- luego muestra toda la información necesaria
completeRoutine :: Program -> RunTime -> IO()
completeRoutine program runt = do
                        let (ert, rest) = vcGenerator program runt
                        let simplifyErt = deepSimplifyRunTime ert
                        let simplifyRest = map (fmap deepSimplifyRunTime) rest
                        let len = length rest
                        let b = len > 0
                        putStr newLine
                        putStrLn "Programa Analizado:"
                        putStrLn $ show program
                        putStr newLine
                        putStrLn "Se calcula la transformada con respecto a"
                        putStrLn $ show runt
                        putStr newLine
                        showTransform simplifyErt simplifyRest len
                        putStr newLine
                        if b
                            then
                            mapM_ (uncurry showSolverInputs ) $ zip simplifyRest [1..len]
                            else putStr newLine
                        putStrLn "Calculo Finalizado"




