module ImpIO where

import Data.SBV
import ImpSBV
import Imp
import ImpVCGen
import Data.List (zip4, zip5, zip6)
import qualified Data.Set as Set
import Control.Monad (zipWithM)


-- Extraído de https://hackage.haskell.org/package/hxt-9.3.1.22/docs/src/Text.XML.HXT.DOM.Util.html#uncurry4
{- MÓDULO QUE SE ENCARGA DE IMPRIMIR LOS RESULTADOS EN LA SALIDA ESTÁNDAR -}

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

-- | Imprime el índice de una obligación de prueba
index :: Int -> String
index n = "[" ++ show n ++ "]"

-- | Imprime el índice de una restricción derivada
index2 :: Int -> Int -> String
index2 n m = "[" ++ show n ++ ", " ++ show m ++ "]"

------------------------------{ MÉTODOS PARA IMPRIMIR LOS RESULTADOS Y MODELOS}----------------------------------------

-- | Imprime si una obligación de prueba es válida o no.
showRestriction :: RRunTime -> Int -> IO Bool -> IO ()
showRestriction x n b = do
                          b' <- b
                          putStrLn $ index n ++ space ++ show x ++ ", " ++ (if b' then  "No es válida"   else  "Es válida")

-- | Imprime un modelo o imprime si no existe uno
showModel :: IO SatResult -> [String] -> IO ()
showModel solution xs = do
                          solution' <- solution
                          let showValue  x =  case (getModelValue x solution' :: Maybe AlgReal) of
                                Just q ->  putStrLn $ x ++  " = " ++ show q ++ " Real"
                                Nothing ->  error "Ha ocurrido un error, por favor revise este caso"
                          mapM_ showValue xs

-- | Dado una restricción derivada, imprime el contraejemplo o si no es válida
showSolverInput :: IO Bool -> IO SatResult -> SolverInput -> Int -> Int ->IO()
showSolverInput b model (contexto, rest, vars) n m = do
      let len = length vars
      b' <- b
      if b'
            then do  putStr newLine
                     putStrLn $ concat (replicate 100 "-")
                     putStr newLine
                     putStrLn $ "Restricción derivada " ++ index2 n m
                     putStrLn $ show contexto ++ "   |-  " ++ show rest
                     putStr newLine
                     putStrLn "La restricción no es válida."
                     putStr newLine
                     if len > 0
                           then do putStrLn "Un contraejemplo encontrado es:"
                                   showModel model vars
                           else putStr newLine
                     putStrLn $ concat (replicate 100 "-")
            else  putStr ""

-- | Imprime los datos asociados a una obligación de prueba
showSolverInputs :: IO Bool -> [IO Bool] -> RRunTime -> [IO SatResult] -> [SolverInput] -> Int -> IO()
showSolverInputs b bs runtr models inputs n = do
                                            b' <- b
                                            if  b'
                                                then do  let m = length inputs
                                                         putStrLn $ concat (replicate 100 "*")
                                                         putStr newLine
                                                         putStrLn $ "Obligación de prueba " ++ index n
                                                         print runtr
                                                         putStr newLine
                                                         putStrLn $ "La obligación de prueba tiene asociada " ++ show m ++ " restricciones derivadas diferentes."
                                                         mapM_ (uncurry5 showSolverInput ) $ zip5 bs models inputs (repeat n) [1..m]
                                                else putStr ""



showRestrictions :: [RRunTime] -> [[SolverInput]] -> [[IO SatResult]] -> [[IO Bool]] -> [IO Bool] -> Bool -> Int -> IO ()
showRestrictions restrictions modelss inputss bss bs b n = do
                                                        if n > 0
                                                            then do putStrLn "Obligaciones de prueba asociadas:"
                                                                    mapM_ (uncurry3 showRestriction) $ zip3 restrictions [1..n] bs
                                                                    putStr newLine
                                                                    if b
                                                                        then do mapM_ (uncurry6 showSolverInputs) $ zip6 bs bss restrictions  inputss modelss [1..n]
                                                                                putStrLn "El tiempo de ejecución calculado no es válido porque alguna obligación de prueba no es válida."
                                                                                putStrLn "Ajuste los invariantes de ciclo y vuelva a realizar el análisis. "
                                                                        else do putStrLn "El tiempo de ejecución calculado es válido porque las obligaciones de prueba son válidas. "

                                                            else do putStrLn "El tiempo de ejecución calculado es válido porque no hay obligaciones de prueba asociadas, ya que el programa no contiene ciclos."

-- | Imprime todos los resultados asociados a un programa
completeRoutine :: Program -> String -> RunTime -> IO()
completeRoutine program str runt = do let (ert, rest, modelss, inputss, bss, bs, b) = routineInput program runt
                                      let len = length rest
                                      b' <- b
                                      putStr newLine
                                      putStrLn "Programa Analizado:"
                                      putStrLn str
                                      putStr newLine
                                      putStrLn "Tiempo de ejecución calculado:"
                                      print ert
                                      putStr newLine
                                      showRestrictions rest inputss modelss bss bs b' len
                                      putStr newLine
                                      putStrLn "Análisis Finalizado."


-------------------- { MISMO FLUJO, MODO NUEVO: SolverInput'/mkUniversales } --------------------
-- El modo antiguo (routineInput/restrictionsToSolver/showRestrictions de más
-- arriba) trata todas las variables como libres y prueba, POR CADA CONTEXTO
-- por separado, si negar la restricción es satisfacible ("proceder por
-- contradicción" — ver el TODO de ImpVCGen.restrictionsToSolver, "este
-- algoritmo es poco claro"). El modo nuevo usa programToSolverInputs: un
-- único problema de SBV por obligación (por while/pwhile), con
-- cuantificación ∃∀ real vía mkUniversales — ∃ (variables de template) tal
-- que ∀ (variables de programa), la obligación completa (todos los
-- contextos a la vez, no uno por uno) se cumple.
--
-- Esto invierte la polaridad de "válida" según si hay variables
-- existenciales o no:
--   * SIN existenciales (invariante ya concreto, el caso típico de
--     ImpProgram.hs): se prueba por contradicción igual que el modo
--     antiguo — Unsatisfiable = no hay contraejemplo = válida;
--     Satisfiable = el modelo ES el contraejemplo = no válida.
--   * CON existenciales (invariante-plantilla con variables sin instanciar,
--     ej. `inv = a**x`): se le pide a Z3 un testigo que valga para todo
--     universal, sin negar — Satisfiable = SÍ existe un testigo = válida (y
--     el modelo trae los valores de template encontrados); Unsatisfiable =
--     ningún testigo funciona = no válida (no hay "contraejemplo" que
--     mostrar, sólo que la forma del template no alcanza).

-- | Imprime los valores de un modelo para una lista de variables (mismo
-- formato que showModel del modo antiguo).
showModel' :: SatResult -> Names -> IO ()
showModel' result = mapM_ showValue
  where
    showValue x = case getModelValue x result :: Maybe AlgReal of
      Just q  -> putStrLn $ x ++ " = " ++ show q ++ " Real"
      Nothing -> error "Ha ocurrido un error, por favor revise este caso"

-- | Imprime y resuelve una obligación de prueba (un SolverInput', uno por
-- while/pwhile). Devuelve si resultó válida, para que showSolverInputs'
-- pueda armar el mensaje final.
showSolverInput' :: Int -> SolverInput' -> IO Bool
showSolverInput' n si = do
  putStr newLine
  putStrLn $ concat (replicate 100 "-")
  putStr newLine
  putStrLn $ "Obligación de prueba " ++ index n
  putStrLn $ "Variables de template (existenciales): " ++ show (Set.toList (existential si))
  putStrLn $ "Variables de programa (universales):   " ++ show (Set.toList (for_all si))
  putStrLn $ "Cantidad de implicaciones: " ++ show (length (solver_formulaes si))
  putStr newLine
  result <- runModel' si
  let hasExistentials = not (Set.null (existential si))
      foundModel       = modelExists result
      valid            = if hasExistentials then foundModel else not foundModel
  if valid
    then putStrLn "La obligación de prueba es válida."
    else if hasExistentials
      then putStrLn "La obligación de prueba no es válida: no existe una asignación de las variables de template que la satisfaga."
      else do putStrLn "La obligación de prueba no es válida."
              putStr newLine
              putStrLn "Un contraejemplo encontrado es:"
              showModel' result (Set.toList (for_all si))
  if valid && hasExistentials
    then do putStr newLine
            putStrLn "Testigo encontrado para las variables de template:"
            showModel' result (Set.toList (existential si))
    else putStr ""
  putStr newLine
  putStrLn $ concat (replicate 100 "-")
  return valid

-- | Imprime todas las obligaciones de prueba de un programa (modo nuevo) y
-- el veredicto final. Devuelve si todas resultaron válidas.
showSolverInputs' :: [SolverInput'] -> IO Bool
showSolverInputs' sis
  | null sis = do
      putStrLn "El tiempo de ejecución calculado es válido porque no hay obligaciones de prueba asociadas, ya que el programa no contiene ciclos."
      return True
  | otherwise = do
      putStrLn "Obligaciones de prueba asociadas:"
      valids <- zipWithM showSolverInput' [1 ..] sis
      putStr newLine
      let allValid = and valids
      if allValid
        then putStrLn "El tiempo de ejecución calculado es válido porque las obligaciones de prueba son válidas."
        else do putStrLn "El tiempo de ejecución calculado no es válido porque alguna obligación de prueba no es válida."
                putStrLn "Ajuste los invariantes de ciclo y vuelva a realizar el análisis."
      return allValid

-- | Igual que completeRoutine, pero con el modo nuevo (programToSolverInputs
-- en vez de routineInput). No recibe un "runt" inicial porque
-- programToSolverInputs/vcGenerator0 ya parten siempre de rtZero (mismo
-- valor que completeRoutine usa en la práctica vía "run" en app/Main.hs).
completeRoutine' :: Program -> String -> IO ()
completeRoutine' program str = do
  let (ert, _rest) = vcGenerator0 program
      sert          = deepSimplifyRunTime ert
      sis           = programToSolverInputs program
  putStr newLine
  putStrLn "Programa Analizado:"
  putStrLn str
  putStr newLine
  putStrLn "Tiempo de ejecución calculado:"
  print sert
  putStr newLine
  _ <- showSolverInputs' sis
  putStr newLine
  putStrLn "Análisis Finalizado."


                                      
