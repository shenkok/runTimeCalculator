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
                                      
{-
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
                        -}