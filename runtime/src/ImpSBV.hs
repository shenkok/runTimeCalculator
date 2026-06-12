module ImpSBV where

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
sAExp env (k :*: arit)  = literal k * sAExp env arit


-- | Constructor de variables booleanas para SBV
sBExp :: ConstantEnv -> BExp -> SBool
sBExp _ True'            = sTrue
sBExp _ False'           = sFalse
sBExp env (e_1 :<=: e_2) = sAExp env e_1 .<= sAExp env e_2
sBExp env (e_1 :==: e_2) = sAExp env e_1 .== sAExp env e_2
sBExp env (e_1 :|: e_2)  = sBExp env e_1 .|| sBExp env e_2
sBExp env (e_1 :&: e_2)  = sBExp env e_1 .&&  sBExp env e_2
sBExp env (Not e)        = sNot (sBExp env e)

-- | Convierte una restricción aritmética en una expresión booleana.
-- En el modelo SBV tratamos las restricciones como la negación de la fórmula
-- porque se usa el método de resolución por contradicción.
raritToBExp :: RArit -> BExp
raritToBExp (a :!<=: b) = Not (a :<=: b)
raritToBExp (a :!==: b) = Not (a :==: b)
raritToBExp (a :<==: b) = a :<=: b
raritToBExp (a :===: b) = a :==: b

sImplication :: ConstantEnv -> Implication -> SBool
sImplication env (Implication hyp concl) = sAnd (map (sBExp env) hyp) .=> sBExp env (raritToBExp concl)

-- | Función que permite reorganizar el input
-- Es útil, ya que las restricciones a!:<=:b no son booleanos, pero se deben tratar como tal
-- por eso la función "f"
-- Además la restricción debe ir negada, ya que se procede por contradicción

reOrganiceInput :: SolverInput -> (Names, Context)
reOrganiceInput (context, rarit, names) = (names, new_context) where
    f (a :!<=:b) = Not (a :<=: b)
    f (a :!==:b) = Not (a :==: b)
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


makeSBVModel :: SolverInput ->  SymbolicT IO ()
makeSBVModel sinput = do
                    let (names, context) = reOrganiceInput sinput
                    xs <- sRationals names
                    let env = M.fromList (zip names xs)
                    constrain (sAnd (map (sBExp env) context))


-- Nueva versión de la función anterior, con el nuevo formato de SolverInput'
-- Si no tengo variables existenciales, procedo por contradicción.

-- makeSBVModel' :: SolverInput' -> SymbolicT IO ()
-- makeSBVModel' (SolverInput' solver_formulaes existentials for_all) = do
--   if null existentials
--     then do
--       ys <- sRationals for_all
--       let env = M.fromList (zip for_all ys)
--       constrain $ sNot (sAnd (map (sImplication env) solver_formulaes))
--     else do
--       xs <- sRationals existentials
--       ys <- mapM forAll for_all
--       let env = M.fromList (zip existentials xs) `M.union` M.fromList (zip for_all ys)
--       constrain $ sAnd (map (sImplication env) solver_formulaes)

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