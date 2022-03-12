module ImpIO where

import Data.SBV
import ImpSBV
import Imp
import ImpVCGen
import Data.List (zip4, zip5, zip6)


-- Estraído de https://hackage.haskell.org/package/hxt-9.3.1.22/docs/src/Text.XML.HXT.DOM.Util.html#uncurry4
{- MÓDULO QUE SE ENCARGA DE IMPRIMIR LOS RESULTADOS EN LA SALIDA ESTANDAR -}

--------------------------{ MÉTODOS UNCURRY }------------------------------------------------------------------------

uncurry3                        :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a, b, c)           = f a b c

uncurry4                        :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(a, b, c, d)        = f a b c d

uncurry5                        :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f ~(a, b, c, d, e)     = f a b c d e

uncurry6                         :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fun ~(a, b, c, d, e, f) = fun a b c d e f

-------------------------------{ MÉTODOS ÚTILES} ---------------------------------------------------------------------

-- | String nueva línea
newLine :: String
newLine = "\n"

-- | String espacio en blanco 
space :: String
space = "  "

-- | imprime el indice de una obligación de prueba
index :: Int -> String
index n = "[" ++ show n ++ "]"

-- | Imprime el indice de un problema lineal
index2 :: Int -> Int -> String
index2 n m = "[" ++ show n ++ ", " ++ show m ++ "]"

------------------------------{ MÉTODOS PARA IMPRIMIR LOS RESULTADOS Y MODELOS}----------------------------------------

-- | Imprime si una obligación de prueba es válida o no.
showRestriction :: RRunTime -> Int -> IO Bool -> IO ()
showRestriction x n b = do
                          b' <- b
                          putStrLn $ index n ++ space ++ show x ++ ", " ++ (if b' then "Es válida" else "No es válida")

-- | Imprime un modelo o imprime si no existe uno
showModel :: IO SatResult -> [String] -> IO ()
showModel solution xs = do
                          solution' <- solution
                          let showValue  x =  case (getModelValue x solution' :: Maybe Rational) of
                                Just q ->  putStrLn $ x ++  " = " ++ showLit q ++ " Racional"
                                Nothing ->  error "A ocurrido un error, por favor revise este caso"
                          mapM_ showValue xs

-- | Dado un problema lineal, imprime el modelo o imprime si no es satisfacible
showSolverInput :: IO Bool -> IO SatResult -> SolverInput -> Int -> Int ->IO()
showSolverInput b model (contexto, rest, vars) n m = do
      let len = length vars
      b' <- b
      if not b'
            then do  putStr newLine
                     putStrLn $ concat (replicate 100 "-")
                     putStr newLine
                     putStrLn $ "Problema lineal " ++ index2 n m
                     putStrLn $ show contexto ++ "   |-  " ++ show rest
                     putStr newLine
                     putStrLn "El problema no es satisfacible."
                     putStr newLine
                     if len > 0
                           then do putStrLn "Un contraejemplo encontrado es:"
                                   showModel model vars
                           else putStr newLine
                     putStrLn $ concat (replicate 100 "-")
            else  putStr ""

-- | Imprime los datos asociados a una oblicación de prueba
showSolverInputs :: IO Bool -> [IO Bool] -> RRunTime -> [IO SatResult] -> [SolverInput] -> Int -> IO()
showSolverInputs b bs runtr models inputs n = do
                                            b' <- b
                                            if not b'
                                                then do  let m = length inputs
                                                         putStrLn $ concat (replicate 100 "*")
                                                         putStr newLine
                                                         putStrLn $ "Obligación de prueba " ++ index n
                                                         print runtr
                                                         putStr newLine
                                                         putStrLn $ "La obligación de prueba tiene asociada " ++ show m ++ " problemas lineales diferentes."
                                                         mapM_ (uncurry5 showSolverInput ) $ zip5 bs models inputs (repeat n) [1..m]
                                                else putStr ""



showRestrictions :: [RRunTime] -> [[SolverInput]] -> [[IO SatResult]] -> [[IO Bool]] -> [IO Bool] -> Bool -> Int -> IO ()
showRestrictions restrictions modelss inputss bss bs b n = do
                                                        if n > 0
                                                            then do putStrLn "Obligaciones de prueba asociadas:"
                                                                    mapM_ (uncurry3 showRestriction) $ zip3 restrictions [1..n] bs
                                                                    putStr newLine
                                                                    if b
                                                                        then do putStrLn "El tiempo de ejecución calculado es válido porque las obligaciones de prueba son válidas. "
                                                                        else do mapM_ (uncurry6 showSolverInputs) $ zip6 bs bss restrictions  inputss modelss [1..n]
                                                                                putStrLn "El tiempo de ejecución calculado no es válido porque alguna obligación de prueba no es válida."
                                                                                putStrLn "Ajuste las invariantes de ciclo y vuelva a realizar el análisis. "
                                                            else do putStrLn "El tiempo de ejecución calculado es válido porque no hay obligaciones de prueba asociadas."

-- | Imprime todos los resultados asociados a un programa
completeRoutine :: Program -> String -> RunTime -> IO()
completeRoutine program str runt = do let (ert, rest, modelss, inputss, bss, bs, b) = routineInput program runt
                                      let len = length rest
                                      b' <- b
                                      putStr newLine
                                      putStrLn "Programa Analizado:"
                                      putStrLn str
                                      putStr newLine
                                      putStrLn "Se calcula la transformada con respecto a:"
                                      print runt
                                      putStr newLine
                                      putStrLn "Tiempo de ejecución calculado:"
                                      print ert
                                      putStr newLine
                                      showRestrictions rest inputss modelss bss bs b' len
                                      putStr newLine
                                      putStrLn "Análisis Finalizado."


                                      
