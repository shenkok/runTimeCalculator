{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImpSBV where
import Data.SBV.Dynamic     -- svMkSymVar, KReal, VarContext(..), Quantifier(..)
import Data.SBV.Internals   -- symbolicEnv
import Control.Monad.IO.Class (liftIO)
import Data.SBV
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe
import Data.Proxy (Proxy (..))
import GHC.TypeNats (SomeNat (..), someNatVal)
import Imp
import Data.SBV.Rational
import ImpVCGen
import Control.Applicative (liftA2)

{-
    MODULO QUE SE ENCARGA DE HACER EL COMPILADOR ENTRE LOS LENGUAJES IMPERATIVOS Y LAS VARIABLES DE SBV
-}


type Env a = M.Map String (SBV a)
type ConstantEnv = Env Constant

-- | Cuantifica universalmente una cantidad arbitraria de variables reales.
--
-- ForallN n "u" AlgReal necesita "n" como Nat de TIPO (DataKinds), resuelto
-- en compilación vía la typeclass Skolemize — por eso antes esta función
-- hacía "case length names of 0/1/2/3 -> ..." con un ForallN literal por
-- cada aridad soportada, y fallaba con error a partir de la cuarta variable.
-- No es un límite de Z3 ni de la teoría (aritmética real cuantificada sigue
-- siendo cara para cualquier n, eso no cambia), es sólo que sólo se habían
-- escrito 4 casos a mano.
--
-- someNatVal empaqueta un Nat existencial (SomeNat) a partir de un número
-- que sólo se conoce en runtime (length names); al desempaquetarlo con
-- (_ :: Proxy k), "k" queda disponible como el mismo tipo de Nat que
-- necesita ForallN, sin importar cuánto valga. Verificado a mano contra Z3
-- con 0 a 8 variables universales (casos sat/unsat esperados, ambos
-- correctos) antes de aplicar este cambio.
mkUniversales :: [String]
              -> (M.Map String SReal -> SBool)
              -> SBool
mkUniversales names f = case someNatVal (fromIntegral (length names)) of
  SomeNat (_ :: Proxy k) ->
    quantifiedBool $ \(ForallN xs :: ForallN k "u" AlgReal) ->
      f (M.fromList (zip names xs))


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
  -- existential/for_all son Set Name (ver ImpVCGen.SolverInput'); sReals y
  -- mkUniversales siguen tomando listas, así que se convierten con toList.
  let universalNamesL = Set.toList universalNames
      existNamesL      = Set.toList existNames
  if null existNames
    then do
      xs <- sReals universalNamesL
      let env = M.fromList (zip universalNamesL xs)
      constrain $ sNot $ sAnd $ map (sImplication env) formulaes
    else do
      xs <- sReals existNamesL
      let existEnv = M.fromList (zip existNamesL xs)
      constrain $ mkUniversales universalNamesL $ \univEnv ->
        let env = existEnv <> univEnv
        in sAnd $ map (sImplication env) formulaes

-- Runner: sat es suficiente porque los cuantificadores ya están en las variables
runModel' :: SolverInput' -> IO SatResult
runModel' = sat . makeSBVModel'

-- Versión monádica IO del and lógico
ioAnd :: IO Bool -> IO Bool -> IO Bool
ioAnd = liftA2 (&&)

-- Versión monádica IO del or lógico
ioOr :: IO Bool -> IO Bool -> IO Bool
ioOr = liftA2 (||)
-- Dado un programa y un runtime entrega el input necesario para poder imprimir los resultados
routineInput :: Program -> RunTime -> (RunTime, [RRunTime],[[IO SatResult]], [[SolverInput]], [[IO Bool]], [IO Bool], IO Bool)
routineInput program runt = (sert, rests, modelss, inputss, bss, bs, b) where
    (ert, rest)    = vcGenerator program runt
    sert           = deepSimplifyRunTime ert
    rests          = map (fmap deepSimplifyRunTime) rest
    inputss        = map restrictionsToSolver rests
    problemss      = map (map makeSBVModel) inputss
    modelss        = map (map sat) problemss
    bss            = map (map isSatisfiable) problemss
    bs             = map (foldr ioOr (pure False)) bss
    b              = foldr ioOr (pure False) bs