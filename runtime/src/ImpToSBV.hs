module ImpToSBV where

import Data.SBV
import qualified Data.Map as M
import Imp

{-dummy-}
{-
    MODULO QUE SE ENCARGA DE HACER EL PARSER ENTRE LOS LENGUAJES IMPERATIVOS Y LAS VARIABLES DE SBV
-}
type Env a = M.Map String (SBV a)
type IntEnv = Env Integer
type FloatEnv = Env Float

-- TODO: Seguir Probando como ahorrarse una función y no hacer dos iguales
-- | Método que retorna una variable SBV para ir construyendo las variables aritméticas
envLookup :: Name -> Env a -> SBV a
envLookup x env = maybe (error $ "Var not found: " ++ show x) id
                            (M.lookup x env)

-- Constructos de variables aritméticas de SBV
-- NOTA: En este caso usé como base los Float, debo seguir investigando
-- para usar de manera rápida y facil la variable entera sin la necesidad de 
-- reescribir todo
aexpFloat :: FloatEnv -> AExp -> SBV Float
aexpFloat _ (Lit n) = literal n 
aexpFloat env (Var x) =  envLookup x env
aexpFloat env (e_1 :+: e_2) = (aexpFloat env e_1 ) + (aexpFloat env e_2)
aexpFloat env (k :*: arit) = (literal k) * (aexpFloat env arit )

{-
aexpInteger :: IntEnv -> AExp -> SBV Integer
aexpInteger _ (Lit n) = literal n 
aexpInteger env (Var x) =  envLookup x env
aexpInteger env (e_1 :+: e_2) = (aexpInteger env e_1 ) + (aexpInteger env e_2)
aexpInteger env (k :*: arit) = (literal k) * (aexpInteger env arit )
-}

-- | Constructor de variables booleanas para SBV
bexp :: FloatEnv -> BExp -> SBool
bexp _ True' = sTrue 
bexp _ False' = sFalse
bexp env (e_1 :<=: e_2) = (aexpFloat env e_1 ) .<= (aexpFloat env e_2 )
bexp env (e_1 :==: e_2) = (aexpFloat env e_1) .== (aexpFloat env e_2 )
bexp env (e_1 :|: e_2) = (bexp env e_1 ) .|| (bexp env e_2)
bexp env (e_1 :&: e_2) = (bexp env e_1 ) .&&  (bexp env e_2)
bexp env (Not e) = sNot (bexp env e)


-- | Función que permite reorganizar el input
-- Es útil, ya que los restictions a!:<=:b no son booleanos, pero se deben tratar como tal
-- por eso la funcion "f"
-- Además la restricción de debe ir negada, ya que se procede por contradicción

reOrganiceInput :: SolverInput -> (Names, Context)
reOrganiceInput (context, rarit, names) = (names, new_context) where
    f (a :!<=:b) = Not (a :<=: b)
    f (a :!==:b) = Not (a :==: b)
    new_rarit = f rarit
    new_context = context ++ [new_rarit]

-- | Función que permite generar el modelo SBV en base a un problema
{- Modela problemas del tipo
        a <- sFloat "a"
        b <- sFloat "b"
        c <- sFloat "c"
        constrain $ a + 10.0 .< 19.0 + b
        constrain $ a + b + c.<= 10 
-}
makeSBVModel :: SolverInput ->  SymbolicT IO ()
makeSBVModel sinput = do
                    let (names, context) = reOrganiceInput sinput
                    xs <- sFloats names
                    let env = M.fromList (zip names xs)
                    constrain $ (sAnd (map (bexp env) context))

