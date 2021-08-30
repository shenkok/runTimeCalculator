module Main where

import ImpToSBV
import Imp
import ImpIO
import Data.Ratio
import Data.SBV
-------------------------------------------------- { EJEMPLOS DE PROGRAMAS} ------------------------------------------------------------------


-- li representa la linea del programa de arriba
-- li_lj representa el programa de la linea i a la j
{-
    Programa 1
0    x:= x - 1
1    if(x>= y)
2        skip
3        y:= 2*x
4    else
5        if(w>= 8)
6            w:= 3
7            x:= w + x
8        else
9            y:= 5
-}

l0 = Set "x" (Var "x":+:Lit (-1))
l2 = Skip
l3 = Set "y" (2:*:Var "x")
l6 = Set "w" (Lit 3)
l7 = Set "x" (Var  "w" :+: Var "x")
l9 = Set "y" (Lit 5)
l5_l9 = If (Lit 8 :<=: Var "w") (Seq l6 l7) l9
l1_l9 = If (Var "y" :<=: Var "x") (Seq l2 l3) l5_l9

programa1 :: Program
programa1 = Seq l0 l1_l9

-- EJEMPLOS
(runt, _) = vcGenerator0 programa1

-- [w < 8]*4 + [w>=8]*5
sumando1 = (Var "w" <: Lit 8) :<>: rtLit 5
sumando2 = (Var "w" >=: Lit 8) :<>: rtLit 8

restriction:: Restriction RunTime
restriction = runt :!<=: (sumando1 :++: sumando2)

input :: IO()
input = showSolverInputs restriction 1


{-
    Programa 2
        while (False) {Invariante: 5}
            Empty
        Invariante correcto
-}
programa2 :: Program
programa2 = While False' Empty (rtLit 5)

{-
    Programa 3
     while(True) {Invariante: 5}
        Skip
        Invariante incorrecto
-}

programa3 :: Program
programa3 = While True' Skip (rtLit 5)

{-
    Programa 4
        while (x >= 0) {Invariante 1 + 2*x}
            x = x- 1

        El invariante no es correcto
-}

invP4 = rtOne :++: RunTimeArit (2 :*:Var "x")
condP4 = Lit 0 :<=: Var "x"
bodyP4 = Set "x" (Var "x" :+: Lit (-1))

programa4 :: Program
programa4 = While condP4 bodyP4 invP4

solution = sat $ do
        a <- sRational "a"
        b <- sRational "b"
        c <- sRational "c"
        constrain $ a + 10.0 .< 19.0 + b
        constrain $ a + b + c.<= 10

main :: IO ()
main = completeRoutine programa4 rtZero