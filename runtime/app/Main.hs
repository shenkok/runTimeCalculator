module Main where

import Lib
import Data.SBV
import ImpToSBV
import Imp
import qualified Data.Map as M


-------------------(EJEMPLOS PARA PROBAR) ----------------------------------

sumando1 = (Not( (Lit 8 ):<=: Var "w")):<>: (RunTimeArit(Lit 4)) 
sumando2 = ( (Lit 8):<=: Var "w"):<>: (RunTimeArit(Lit 5))

runtr = sumando1 :++: sumando2

res = s1 :!<=: runtr

ejemploP3 = restrictionsToSolver res

ejSol0 = ejemploP3 !! 0
(names0, contexto0) =  reOrganiceInput ejSol0

ejSol1 = ejemploP3 !! 1
(names1, contexto1) =  reOrganiceInput ejSol1

ejSol2 = ejemploP3 !! 2
(names2, contexto2) =  reOrganiceInput ejSol2

ejSol3 = ejemploP3 !! 3
(names3, contexto3) =  reOrganiceInput ejSol3

printEj = showSolverInputs res


-------------------(EJEMPLOS PARA PROBAR) ----------------------------------

--------------------------------- (Antes y Despues)--------------------------------------------
namesSolution = ["a", "b"]
restSol1 = ((Var "a") :+: (Var "b")) :<=: (Lit 0)
restSol2 = (Var "b") :<=: (Lit 5)
contextoSol = [restSol1, restSol2]
solution = isSatisfiable $ do 
    a <- sInteger "a"
    b <- sInteger "b"
    constrain $ a + b .<= 0
    constrain $ b .<= 5

solution' = sat $ do 
    [a, b] <- sIntegers ["a", "b"]
    constrain $ (sAnd [a + b .<= 0,  b .<= 5])

solution'' = sat $ do
    xs <- sFloats namesSolution 
    let env = M.fromList (zip namesSolution xs)
    constrain $ (sAnd (map (bexp env) contextoSol))

solution0 = sat $ do
    y <- sInteger "y"
    x <- sInteger "x"
    w <- sInteger "w"
    constrain $ sNot (y .<= 1 + x )
    constrain $ sNot (8.<= w)
    constrain $ (sNot $ (4::SInteger) .<= 4)


solution0' = sat $ do 
    xs <- sFloats names0
    let env = M.fromList (zip names0 xs)
    constrain $ (sAnd (map (bexp env) contexto0))
solution1' = sat $ do 
    xs <- sFloats names1
    let env = M.fromList (zip names1 xs)
    constrain $ (sAnd (map (bexp env) contexto1))

solution2' = isSatisfiable $ do 
    xs <- sFloats names2
    let env = M.fromList (zip names2 xs)
    constrain $ (sAnd (map (bexp env) contexto2))

solution3' = sat $ do 
    xs <- sFloats names3
    let env = M.fromList (zip names3 xs)
    constrain $ (sAnd (map (bexp env) contexto3))

main :: IO ()
main = putStrLn "Compila!!!"