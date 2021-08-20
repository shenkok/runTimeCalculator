module ImpToSBV where

import Data.SBV
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Foldable (foldMap)
import Control.Monad ((<=<))
import Imp

type Env = M.Map String SFloat


envLookup :: Name -> Env -> SFloat
envLookup x env = maybe (error $ "Var not found: " ++ show x) id
                            (M.lookup x env)

aexp :: Env -> AExp -> SFloat
aexp _ (Lit n) =  literal n
aexp env (Var x) =  envLookup x env
aexp env (e_1 :+: e_2) = (aexp env e_1 ) + (aexp env e_2)
aexp env (k :*: arit) = (literal k) * (aexp env arit )

bexp :: Env -> BExp -> SBool
bexp _ True' = sTrue 
bexp _ False' = sFalse
bexp env (e_1 :<=: e_2) = (aexp env e_1 ) .<= (aexp env e_2 )
bexp env (e_1 :==: e_2) = (aexp env e_1) .== (aexp env e_2 )
bexp env (e_1 :|: e_2) = (bexp env e_1 ) .|| (bexp env e_2)
bexp env (e_1 :&: e_2) = (bexp env e_1 ) .&&  (bexp env e_2)
bexp env (Not e) = sNot (bexp env e)

reOrganiceInput :: SolverInput -> (Names, Context)
reOrganiceInput (context, rarit, names) = (names, new_context) where
    f (a :!<=:b) = Not (a :<=: b)
    f (a :!==:b) = Not (a :==: b)
    new_rarit = f rarit
    new_context = context ++ [new_rarit]



