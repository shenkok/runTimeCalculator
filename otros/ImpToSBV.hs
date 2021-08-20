module ImpToSBV where

import Imp
import Data.Map
import Data.List
import Data.SBV

type Vars = Map Name (Symbolic SInteger)
names = ["a", "b"]
makeVars :: Names -> Vars
makeVars names = fromList (zip names vars) where
    vars = fmap (sInteger) names

aexp :: AExp -> Vars -> SInteger
aexp (Lit n) _ = (fromIntegral (n::Int)) :: SInteger
aexp (Var x) env =  env ! x
aexp (e_1 :+: e_2) env = (aexp e_1 env) + (aexp e_2 env)
aexp (k :*: arit) env = ((fromIntegral (k::Int)) :: SInteger) * (aexp arit env)

bexp :: BExp -> Vars -> SBool
bexp True' _ = sTrue 
bexp False' _ = sFalse
bexp (e_1 :<=: e_2) env = (aexp e_1 env) .<= (aexp e_2 env)
bexp (e_1 :==: e_2) env = (aexp e_1 env) .== (aexp e_2 env)
bexp (e_1 :|: e_2) env = (bexp e_1 env) .|| (bexp e_2 env)
bexp (e_1 :&: e_2) env = (bexp e_1 env) .&&  (bexp e_2 env)
bexp (Not e) env = sNot (bexp e env)

