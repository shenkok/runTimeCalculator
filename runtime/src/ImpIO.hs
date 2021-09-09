module ImpIO where

import Data.SBV
import ImpToSBV
import Imp
import ImpToSBVInput
{-
    MODULO QUE SE ENCARGA SE IMPRIMIR LAS DIFERENTES VARIABLES DEL PROBLEMA ESTUDIADO
-}
index :: Int -> String
index n = "[" ++ (show n) ++ "]"

index2 :: (Int, Int) -> String
index2 (n, m) = "[" ++ (show n) ++ ", " ++ (show m) ++ "]"

showRestrictions :: [RRunTime] -> Int -> String
showRestrictions [] _     = newLine
showRestrictions (x:xs) n = (index n) ++ space ++ (show x) ++ (showRestrictions xs (n-1))  

-- | Muestra la transformada calculada y las restricciones que se generaron
showTransform :: RunTime -> [RRunTime] -> Int ->IO()
showTransform ert restrictions n = if n > 0
                                       then do 
                                          putStrLn "Tiempo de ejecución calculado:"
                                          putStrLn $ show ert
                                          putStrLn "Las restricciones son:"
                                          putStr newLine
                                          putStrLn $ showRestrictions restrictions n
                                          putStrLn $ concat (replicate 100 "-")
                                        else
                                          putStrLn "No hay restricciones asociadas"


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
-- La n denota el subproblema n
showSolverInput :: SolverInput -> (Int, Int) ->IO()
showSolverInput (contexto, rest, vars) indice = do
  let model = makeSBVModel (contexto, rest, vars)
  let lenb = not (null vars)
  b <- isSatisfiable model
  putStr newLine
  putStrLn $ "El sub-problema indice " ++ (index2 indice) 
  putStr newLine
  putStrLn "Las variables libres son :"
  print vars
  putStr newLine
  putStrLn "El contexto es :"
  print contexto
  putStr newLine
  putStrLn "La restricción es :"
  print rest
  putStr newLine
  if b
      then do   values <- sat model
                putStrLn "El problema no es satisfacible"
                if lenb
                  then do
                      putStrLn "Un contraejemplo es"
                      print values
                  else
                    putStrLn ""
      else putStrLn "El problema es satisfacible"
  putStrLn $ concat (replicate 100 "-")

-- | Muestra un arreglo de problemas, n es un entero de denota la restricion n
showSolverInputs ::RRunTime -> Int -> IO()
showSolverInputs runtr n = do
  putStrLn $ concat (replicate 100 "*")
  putStr newLine
  putStrLn $ "Para la restricción número " ++ index n
  putStrLn $ (show runtr)
  putStr newLine 
  putStrLn $ "Hay un total de " ++ show l ++ " sub-problemas diferentes." 
  putStr newLine
  putStrLn $ concat (replicate 100 "-")
  mapM_ (uncurry showSolverInput ) $ zip  inputs (zip (repeat n) [1..l])
  putStrLn $ concat (replicate 100 "*")
  where
    inputs = restrictionsToSolver runtr
    l = length inputs


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




