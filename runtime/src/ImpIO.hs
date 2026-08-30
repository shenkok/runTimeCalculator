module ImpIO where

import Control.Monad (forM_, unless, when)
import Data.SBV
import ImpSBV
import Imp

{- MÓDULO QUE SE ENCARGA DE IMPRIMIR LOS RESULTADOS EN LA SALIDA ESTÁNDAR -}

-------------------------------{ MÉTODOS ÚTILES} ---------------------------------------------------------------------

-- | String espacio en blanco
space :: String
space = "  "

-- | Imprime el índice de una obligación de prueba
index :: Int -> String
index n = "[" ++ show n ++ "]"

-- | Imprime el índice de una restricción derivada
index2 :: Int -> Int -> String
index2 n m = "[" ++ show n ++ ", " ++ show m ++ "]"

-- | Imprime una línea separadora hecha del carácter dado
separator :: Char -> IO ()
separator c = putStrLn (replicate 100 c)

-- | "Es válida" / "No es válida" según corresponda
validity :: Bool -> String
validity invalid = if invalid then "No es válida" else "Es válida"

------------------------------{ MÉTODOS PARA IMPRIMIR LOS RESULTADOS Y MODELOS}----------------------------------------

-- | Imprime el valor de cada variable libre en un contraejemplo
showModel :: SatResult -> [Name] -> IO ()
showModel model = mapM_ showValue
  where
    showValue x = case getModelValue x model :: Maybe AlgReal of
      Just q  -> putStrLn $ x ++ " = " ++ show q ++ " Real"
      Nothing -> error "Ha ocurrido un error, por favor revise este caso"

-- | Imprime el contraejemplo de una restricción derivada, si no es válida
showDerivedResult :: Int -> Int -> DerivedResult -> IO ()
showDerivedResult n m (DerivedResult (contexto, rest, vars) model invalidM) = do
  invalid <- invalidM
  when invalid $ do
    putStrLn ""
    separator '-'
    putStrLn ""
    putStrLn $ "Restricción derivada " ++ index2 n m
    putStrLn $ show contexto ++ "   |-  " ++ show rest
    putStrLn ""
    putStrLn "La restricción no es válida."
    putStrLn ""
    unless (null vars) $ do
      putStrLn "Un contraejemplo encontrado es:"
      model' <- model
      showModel model' vars
    separator '-'

-- | Imprime los datos asociados a una obligación de prueba no válida
showObligationDetail :: Int -> ObligationResult -> IO ()
showObligationDetail n (ObligationResult runt derived invalidM) = do
  invalid <- invalidM
  when invalid $ do
    separator '*'
    putStrLn ""
    putStrLn $ "Obligación de prueba " ++ index n
    print runt
    putStrLn ""
    putStrLn $ "La obligación de prueba tiene asociada " ++ show (length derived) ++ " restricciones derivadas diferentes."
    forM_ (zip [1 ..] derived) $ \(m, d) -> showDerivedResult n m d

-- | Imprime la lista de obligaciones de prueba y, si alguna no es válida, su detalle
showObligations :: Bool -> [ObligationResult] -> IO ()
showObligations _ [] =
  putStrLn "El tiempo de ejecución calculado es válido porque no hay obligaciones de prueba asociadas, ya que el programa no contiene ciclos."
showObligations invalid obligations = do
  putStrLn "Obligaciones de prueba asociadas:"
  forM_ (zip [1 ..] obligations) $ \(n, ob) -> do
    invalid' <- obInvalid ob
    putStrLn $ index n ++ space ++ show (obRestriction ob) ++ ", " ++ validity invalid'
  putStrLn ""
  if invalid
    then do
      forM_ (zip [1 ..] obligations) $ \(n, ob) -> showObligationDetail n ob
      putStrLn "El tiempo de ejecución calculado no es válido porque alguna obligación de prueba no es válida."
      putStrLn "Ajuste los invariantes de ciclo y vuelva a realizar el análisis. "
    else putStrLn "El tiempo de ejecución calculado es válido porque las obligaciones de prueba son válidas. "

-- | Imprime todos los resultados asociados a un programa
completeRoutine :: Program -> String -> RunTime -> IO ()
completeRoutine program str runt = do
  let AnalysisResult sert obligations invalidM = routineInput program runt
  invalid <- invalidM
  putStrLn ""
  putStrLn "Programa Analizado:"
  putStrLn str
  putStrLn ""
  putStrLn "Tiempo de ejecución calculado:"
  print sert
  putStrLn ""
  showObligations invalid obligations
  putStrLn ""
  putStrLn "Análisis Finalizado."
