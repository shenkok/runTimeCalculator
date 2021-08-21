module Main where

import Lib
import Data.SBV
import ImpToSBV
import Imp
import qualified Data.Map as M
import ImpIO

-------------------------------------------------- { EJEMPLOS DE PROGRAMAS} ------------------------------------------------------------------
zero :: RunTime 
zero = RunTimeArit (Lit 0)

runTArit :: Float -> RunTime
runTArit k = RunTimeArit (Lit k)

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

l0 = Set "x" ((Var "x"):+:(Lit $ -1))
l2 = Skip
l3 = Set "y" (2:*:(Var "x"))
l6 = Set "w" (Lit 3)
l7 = Set "x" ((Var  "w") :+: (Var "x"))
l9 = Set "y" (Lit 5)
l5_l9 = If ((Lit 8) :<=: (Var "w")) (Seq l6 l7) l9
l1_l9 = If ((Var "y") :<=: (Var "x")) (Seq l2 l3) l5_l9
programa1 :: Program 
programa1 = Seq l0 l1_l9

{-
    Programa 2
        while (False) {Invariante: 5}
            Empty
    Invariante correcto
-}
programa2 :: Program
programa2 = (While False' Empty (runTArit 5)))

{-
    Programa 3
     while(True) {Invariante: 5}
        Skip
    Invariante incorrecto
-}

programa3 :: Program
programa3 = (While True' Skip (runTArit 5)))


solution = sat $ do
    a <- sInteger "a"
    b <- sInteger "b"
    constrain $ a .< b + 10

main :: IO ()
main = completeRoutine programa3 zero