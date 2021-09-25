module Main where

import ImpToSBV
import Imp
import ImpIO
import ImpToSBVInput
import Data.Ratio
import Data.SBV
import Data.SBV.Rational
import ImpParser
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

{-
    p*<true> + (1-p)*<false> = ber p
    if (ber 1/2){
        succ:= true
        }
        else {
            if (ber 1/2){
                succ:=true
                }
            else {
                succ:=false
            }
        }
-}
programa5 :: Program
programa5 = PIf (Ber 0.5)
                (Set "succ" (Lit 1))
                (PIf (Ber 0.5)
                    (Set "succ" (Lit 1))
                    (Set "succ" (Lit 0)))
{-
    while (c=1) {Invariante = 1 + [c=1]*4}
        c:~ 1/2*0 + 1/2*1
-}
 
invariante6 :: RunTime
invariante6 = rtOne :++: ((Var "c":==: Lit 1) :<>: rtLit 4)  
programa6 :: Program
programa6 = While (Var "c":==: Lit 1) (PSet "c" (uniformN 2)) invariante6

arit_1 = regularParse aexp "9 + 10 + 11"
arit_2 = regularParse aexp "9*p0 + 10/6 - 11"
arit_3 = regularParse aexp "9*p - 10/6*x - 11"
arit_4 = regularParse aexp "-11"

rtarit_1 = regularParse runtime "9 ++ 10 ++ 11"
rtarit_2 = regularParse runtime "9*p ++ 10/6 -- 11"
rtarit_3 = regularParse runtime "9*p -- 10/6*x -- 11"
rtarit_4 = regularParse runtime "-11"
ind_1 = regularParse runtime "[true]"
ind_2 = regularParse runtime "[a <= x] ++ 33 -- 23 "
ind_3 = regularParse runtime "2**[f == 3*x + 5/2] <> w"
--ind_2 = regularParse 

main :: IO ()
main =  completeRoutine programa4 rtZero