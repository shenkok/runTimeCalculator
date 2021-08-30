module ImpIO where

import Data.SBV
import ImpToSBV
import Imp

{-
    MODULO QUE SE ENCARGA SE IMPRIMIR LAS DIFERENTES VARIABLES DEL PROBLEMA ESTUDIADO
-}

-- | Muestra la transformada calculada y las restricciones que se generaron
showTransform :: RunTime -> [RRunTime] ->IO()
showTransform ert restrictions = do
                                let b = not (null restrictions)
                                putStrLn "La transformada calculada es:"
                                print ert
                                if b
                                    then do
                                    putStrLn "Las restricciones son:"
                                    print restrictions
                                    else putStrLn "No hay restricciones"
                                print $ concat (replicate 50 "-")

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
showSolverInput :: SolverInput -> Int -> IO()
showSolverInput (contexto, rest, vars) n = do
  let model = makeSBVModel (contexto, rest, vars)
  let lenb = not (null vars)
  b <- isSatisfiable model
  putStr "El sub-problema número "
  print n
  putStrLn "Las variables libres son :"
  print vars
  putStrLn "El contexto es :"
  print contexto
  putStrLn "La restricción es :"
  print rest
  putStrLn "Se debe probar que contexto => restriccion "
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
  print $ concat (replicate 50 "-")

-- | Muestra un arreglo de problemas, n es un entero de denota la restricion n
showSolverInputs ::RRunTime -> Int -> IO()
showSolverInputs runtr n = do
  print $ concat (replicate 50 "*")
  putStrLn $ "La restricción " ++ show n ++" es :"
  print runtr
  putStr "Hay un total de "
  putStr.show $ l
  putStrLn " sub-problemas diferentes  "
  print $ concat (replicate 50 "-")
  mapM_ (uncurry showSolverInput ) $ zip  inputs [1..l]
  print $ concat (replicate 50 "*")
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
                        putStrLn "El programa es:"
                        print program
                        putStrLn "Se calcula la transformada con respecto a"
                        print runt
                        putStrLn ""
                        showTransform simplifyErt simplifyRest
                        putStrLn " Se procede a analizar las restricciones"
                        putStrLn ""
                        if b
                            then
                            mapM_ (uncurry showSolverInputs ) $ zip simplifyRest [1..len]
                            else putStrLn ""
                        putStrLn "Calculo Finalizado"




