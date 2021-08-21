module Main where

import Lib
import Data.SBV
import ImpToSBV
import Imp
import qualified Data.Map as M
import ImpIO

-------------------------------------------------- { EJEMPLOS DE PROGRAMAS} ------------------------------------------------------------------

{-
    Programa 1
    x:= x - 1
    if(x>= y)
        skip
        y:= 2*x
    else 
        if(w>= 8)
            w:= 3
            x:= w + x
        else 
            y:= 5
-}




main :: IO ()
main = putStrLn "Compila"