module ImpToIO where

import Data.SBV
import ImpSBV
import Imp
import ImpVCGen
import Data.List (zip4)

-- Estraído de https://hackage.haskell.org/package/hxt-9.3.1.22/docs/src/Text.XML.HXT.DOM.Util.html#uncurry4
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

showRestriction :: RRunTime -> Int -> IO Bool -> IO ()
showRestriction x n b = do
                          b' <- b
                          putStrLn $ (index n) ++ space ++ (show x) ++ ", " ++ (if b' then "Es satisfacible" else "No es satisfacible")

showModel :: IO SatResult -> [String] -> IO ()
showModel solution xs = do
                          solution' <- solution
                          let showValue  x =  case (flip getModelValue solution' x :: Maybe Rational) of 
                                Just q ->  putStrLn $ x ++ "= " ++ (showLit q) ++ " Racional"
                                Nothing ->  error "A ocurrido un error, por favor revise este caso"
                          mapM_ showValue xs


showSolverInput :: IO SatResult -> SolverInput -> Int -> Int ->IO()
showSolverInput model (contexto, rest, vars) n m = do
  putStr newLine
  putStrLn $ "Sub-problema " ++ (index2 n m)
  putStr newLine 
  putStrLn $ show contexto ++ " ----> " ++ show rest
  putStr newLine
  putStrLn "El problema no es satisfacible"
  putStrLn "Un contraejemplo encontrado es:"
  showModel model vars
  putStrLn $ concat (replicate 100 "-")


showSolverInputs :: RRunTime -> [IO SatResult] -> [SolverInput] -> Int -> IO()
showSolverInputs runtr models inputs n = do
  let m = length inputs
  putStrLn $ concat (replicate 100 "*")
  putStr newLine
  putStrLn $ "Para la obligación de prueba " ++ index n
  putStrLn $ (show runtr)
  putStr newLine 
  putStrLn $ "Hay un total de " ++ show m ++ " sub-problemas diferentes." 
  putStrLn $ concat (replicate 100 "-")
  mapM_ (uncurry4 showSolverInput ) $ zip4 models inputs (repeat n) [1..m]
  putStrLn $ concat (replicate 100 "*")

-- | Muestra la transformada calculada y las restricciones que se generaron

showRestrictions :: [RRunTime] -> [IO Bool] -> Bool -> Int -> IO ()
showRestrictions restrictions bs b n = if n > 0
                                          then do
                                          putStrLn "Obligaciones de prueba asociadas:"
                                          mapM_ (uncurry3 showRestriction) $ zip3 restrictions [1..n] bs
                                          putStr newLine
                                          if b
                                              then do
                                              putStrLn "Las obligaciones de prueba son satisfacibles"
                                              else 
                                                putStrLn "Existen invariantes incorrectos, por a continuación se dará los contraejemplos encontrados"
                                          else putStrLn "No hay obligaciones de prueba asociadas"


completeRoutine :: Program -> String -> RunTime -> IO()
completeRoutine program str runt = do
                        let (ert, rest, model_problems, inputs, b_problems, b_rs, b_all) = routineInput program runt
                        let len = length rest
                        b_all' <- b_all
                        putStr newLine
                        putStrLn "Programa Analizado:"
                        putStrLn str
                        putStr newLine
                        putStrLn "Se calcula la transformada con respecto a"
                        putStrLn $ show runt
                        putStr newLine
                        putStrLn "Tiempo de ejecución calculado:"
                        putStrLn $ show ert
                        putStr newLine
                        showRestrictions rest b_rs b_all' len
                        putStr newLine
                        putStrLn "Calculo Finalizado"

