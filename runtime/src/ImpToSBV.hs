module ImpToSBV where

import Data.SBV
import qualified Data.Map as M
import Data.Maybe
import Imp
import Data.SBV.Rational
import ImpToSBVInput
{-
    MODULO QUE SE ENCARGA DE HACER EL PARSER ENTRE LOS LENGUAJES IMPERATIVOS Y LAS VARIABLES DE SBV
-}
type Env a = M.Map String (SBV a)
type ConstantEnv = Env Constant
-- | Método que retorna una variable SBV para ir construyendo las variables aritméticas
envLookup :: Name -> Env a -> SBV a
envLookup x env = fromMaybe (error $ "Var not found: " ++ show x)
                            (M.lookup x env)

-- Constructos de variables aritméticas de SBV
aexp :: ConstantEnv -> AExp -> SBV Constant
aexp  _ (Lit q)        = (literal $ numerator q) .% (literal $ denominator q)
aexp env (Var x)       = envLookup x env
aexp env (e_1 :+: e_2) = aexp env e_1 + aexp env e_2
aexp env (k :*: arit)  = literal k * aexp env arit


-- | Constructor de variables booleanas para SBV
bexp :: ConstantEnv -> BExp -> SBool
bexp _ True'            = sTrue
bexp _ False'           = sFalse
bexp env (e_1 :<=: e_2) = aexp env e_1 .<= aexp env e_2
bexp env (e_1 :==: e_2) = aexp env e_1 .== aexp env e_2
bexp env (e_1 :|: e_2)  = bexp env e_1 .|| bexp env e_2
bexp env (e_1 :&: e_2)  = bexp env e_1 .&&  bexp env e_2
bexp env (Not e)        = sNot (bexp env e)


-- | Función que permite reorganizar el input
-- Es útil, ya que los restictions a!:<=:b no son booleanos, pero se deben tratar como tal
-- por eso la funcion "f"
-- Además la restricción de debe ir negada, ya que se procede por contradicción

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
                    constrain (sAnd (map (bexp env) context))

