{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImpSBV where
import Data.SBV.Dynamic     -- svMkSymVar, KReal, VarContext(..), Quantifier(..)
import Data.SBV.Internals   -- symbolicEnv
import Control.Monad.IO.Class (liftIO)
import Data.SBV
import qualified Data.Map as M
import Data.Maybe
import Imp
import Data.SBV.Rational
import ImpVCGen
import Control.Applicative (liftA2)

{-
    MODULO QUE SE ENCARGA DE HACER EL COMPILADOR ENTRE LOS LENGUAJES IMPERATIVOS Y LAS VARIABLES DE SBV
-}


type Env a = M.Map String (SBV a)
type ConstantEnv = Env Constant

mkUniversales :: [String] 
              -> (M.Map String SReal -> SBool) 
              -> SBool
mkUniversales names f = case length names of
  0 -> f M.empty
  1 -> quantifiedBool $ \(ForallN xs :: ForallN 1 "u" AlgReal) ->
         f (M.fromList (zip names xs))
  2 -> quantifiedBool $ \(ForallN xs :: ForallN 2 "u" AlgReal) ->
         f (M.fromList (zip names xs))
  3 -> quantifiedBool $ \(ForallN xs :: ForallN 3 "u" AlgReal) ->
         f (M.fromList (zip names xs))
  _ -> error "Máximo 3 variables universales soportadas"


-- | Lookup seguro de variable SBV en el entorno
lookupEnv :: Name -> Env a -> Maybe (SBV a)
lookupEnv = M.lookup

-- | Método que retorna una variable SBV para ir construyendo las variables aritméticas
envLookup :: Name -> Env a -> SBV a
envLookup x env =
  fromMaybe
    (error $ "ImpSBV.envLookup: variable no definida en el entorno: " ++ show x)
    (lookupEnv x env)

-- Constructor de variables aritméticas de SBV
sAExp :: ConstantEnv -> AExp -> SBV Constant
sAExp  _ (Lit q)        = literal q
sAExp env (Var x)       = envLookup x env
sAExp env (e_1 :+: e_2) = sAExp env e_1 + sAExp env e_2
sAExp env (e_1 :*: e_2) = sAExp env e_1 * sAExp env e_2


-- | Constructor de variables booleanas para SBV
sBExp :: ConstantEnv -> BExp -> SBool
sBExp _ True'            = sTrue
sBExp _ False'           = sFalse
sBExp env (e_1 :<=: e_2) = sAExp env e_1 .<= sAExp env e_2
sBExp env (e_1 :==: e_2) = sAExp env e_1 .== sAExp env e_2
sBExp env (e_1 :|: e_2)  = sBExp env e_1 .|| sBExp env e_2
sBExp env (e_1 :&: e_2)  = sBExp env e_1 .&&  sBExp env e_2
sBExp env (Imp.Not e)        = sNot (sBExp env e)

-- | Constructor de formulas implica para SBV
sImplication :: ConstantEnv -> Implication -> SBool
sImplication env (Implication hyp concl) = sAnd (map (sBExp env) hyp) .=> sBExp env concl

-- | Función que permite reorganizar el input
-- Es útil, ya que las restricciones a:<==:b no son booleanos, pero se deben tratar como tal
-- por eso la función "f"
-- Retorno la restriccion negada ya que procedo por contradicción

reOrganiceInput :: SolverInput -> (Names, Context)
reOrganiceInput (context, rarit, names) = (names, new_context) where
    f (a :<==:b) = Imp.Not $ a :<=: b -- Procedo por contradicción
    new_rarit    = f rarit
    new_context  = context ++ [new_rarit]

-- | Función que permite generar el modelo SBV en base a un problema
{- Modela problemas del tipo
        a <- sRational "a"
        b <- sRational "b"
        c <- sFloat "c"
        constrain $ a + 10.0 .< 19.0 + b
        constrain $ a + b + c.<= 10 
-}


makeSBVModel :: SolverInput -> SymbolicT IO ()
makeSBVModel sinput = do
                    let (names, context) = reOrganiceInput sinput
                    xs <- sReals names
                    let env = M.fromList (zip names xs)
                    constrain (sAnd (map (sBExp env) context))

makeSBVModel' :: SolverInput' -> SymbolicT IO ()
makeSBVModel' SolverInput'{ existential     = existNames
                           , for_all          = universalNames
                           , solver_formulaes = formulaes     } = do
  if null existNames
    then do
      xs <- sReals universalNames
      let env = M.fromList (zip universalNames xs)
      constrain $ sNot $ sAnd $ map (sImplication env) formulaes
    else do
      xs <- sReals existNames
      let existEnv = M.fromList (zip existNames xs)
      constrain $ mkUniversales universalNames $ \univEnv ->
        let env = existEnv <> univEnv
        in sAnd $ map (sImplication env) formulaes

-- Runner: sat es suficiente porque los cuantificadores ya están en las variables
runModel' :: SolverInput' -> IO SatResult
runModel' = sat . makeSBVModel'

-- Versión monádica IO del or lógico
ioOr :: IO Bool -> IO Bool -> IO Bool
ioOr = liftA2 (||)

-- | True si alguno de los resultados es True (False para la lista vacía).
anyIO :: [IO Bool] -> IO Bool
anyIO = foldr ioOr (pure False)

-- | Resultado de verificar una restricción derivada (un caso de la
-- linealización de una obligación de prueba) con el SMT-solver.
data DerivedResult = DerivedResult
  { drInput   :: SolverInput  -- ^ problema enviado al solver
  , drModel   :: IO SatResult -- ^ modelo (contraejemplo) encontrado por Z3
  , drInvalid :: IO Bool      -- ^ True si el solver encontró un contraejemplo
  }

-- | Resultado de verificar una obligación de prueba, junto con las
-- restricciones derivadas que la componen.
data ObligationResult = ObligationResult
  { obRestriction :: RRunTime
  , obDerived     :: [DerivedResult]
  , obInvalid     :: IO Bool  -- ^ True si alguna restricción derivada no es válida
  }

-- | Resultado completo del análisis de un programa: el tiempo de ejecución
-- calculado, sus obligaciones de prueba, y si alguna de ellas no es válida.
data AnalysisResult = AnalysisResult
  { arRuntime     :: RunTime
  , arObligations :: [ObligationResult]
  , arInvalid     :: IO Bool
  }

-- | Dado un programa y un runtime entrega el input necesario para poder imprimir los resultados
routineInput :: Program -> RunTime -> AnalysisResult
routineInput program runt = AnalysisResult sert obligations (anyIO (map obInvalid obligations)) where
    (ert, rest) = vcGenerator program runt
    sert        = deepSimplifyRunTime ert
    obligations = map toObligation rest

    toObligation r = ObligationResult r' derived (anyIO (map drInvalid derived)) where
        r'      = deepSimplifyRunTime <$> r
        derived = map toDerived (restrictionsToSolver r')

    toDerived input = DerivedResult input (sat problem) (isSatisfiable problem) where
        problem = makeSBVModel input