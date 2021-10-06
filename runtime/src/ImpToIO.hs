module ImpToIO where

import Data.SBV
import ImpToSBV
import Imp
import ImpToSBVInput

uncurry3                        :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a, b, c)           = f a b c

uncurry4                        :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(a, b, c, d)        = f a b c d

boolIO :: Bool -> IO Bool
boolIO = pure

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

showModel :: IO SatResult -> [String] -> IO ()
showModel solution xs = do
    solution' <- solution
    let showValue  x = putStrLn $ x ++ ":= " ++ (showLit $ fromMaybe 0 (flip getModelValue solution' x :: Maybe Rational))
    mapM_ showValue xs

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

showSolverInput :: IO SatResult -> SolverInput -> IO Bool -> Int -> Int ->IO()
showSolverInput model (contexto, rest, vars) b n m = do
  b'<- b
  putStr newLine
  putStrLn $ "Sub-problema " ++ (index2 n m) 
  putStr newLine 
  putStrLn $ show contexto ++ " ----> " ++ show rest
  putStr newLine
  if b'
      then do 
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

showSolverInputs :: RRunTime -> [IO SatResult] -> [SolverInput] -> [IO Bool] -> Int -> IO() -
showSolverInputs runtr models inputs bs n = do
  let m = length inputs
  let b_restrict = foldl ioAnd (pure True) bs
  putStrLn $ concat (replicate 100 "*")
  putStr newLine
  putStrLn $ "Para la obligación de prueba " ++ index n
  putStrLn $ (show runtr)
  putStr newLine 
  putStrLn $ "Hay un total de " ++ show m ++ " sub-problemas diferentes." 
  putStrLn $ concat (replicate 100 "-")
  mapM_ (uncurry3 showSolverInput ) $ zip3 models inputs bs (repeat n) [1..m]
  putStrLn "La obligación de prueba " ++ (show runtr)
  if b_restrict
    then do
      putStrLn "Es satisfacible"
    else
      putStrLn "No es satisfacible"
      putStrLn "Revise el invariante y vuelva a intentarlo"  
  putStrLn $ concat (replicate 100 "*")


completeRoutine :: Program -> RunTime -> IO()
completeRoutine program runt = do
                        let (ert, rest, model_problems, inputs, b_problems) = routineInput program runt
                        let len        = length rest
                        let b          = len > 0
                        let b_restrict = map (foldl ioAnd (pure True)) b_problems
                        let b_all      = foldl ioAnd (pure True) b_restrict
                        putStr newLine
                        putStrLn "Programa Analizado:"
                        putStrLn $ show program
                        putStr newLine
                        putStrLn "Se calcula la transformada con respecto a"
                        putStrLn $ show runt
                        putStr newLine
                        showTransform ert rest len
                        putStr newLine
                        b_program <- b_all
                        if b
                            then do
                            mapM_ (uncurry showSolverInputs ) $ zip rest model_problems inputs b_problems [1..len]
                            if b_program
                              then
                              putStrLn "Las obligaciones de pruebas son satisfacibles :"
                              else putStrLn "Existen invariantes NO válidos, por favor reviselos e intentelo otra vez"
                            else putStr newLine
                        putStrLn "Calculo Finalizado"
