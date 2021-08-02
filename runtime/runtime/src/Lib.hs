module Lib
    ( someFunc
    ) where

import Data.SBV
import Data.List

prueba :: Integer -> Integer
prueba x = 2*x

{-
solution = sat $ do
    a <- sInteger "a"
    b <- sInteger "b"
    c <- sInteger "c"
    constrain $ (sNot $ 0 .> a + 0) .&& a .< b .&& b .< c
    constrain $ a*a + b*b .== c*c 
    constrain $ a + b + c .== 1000 
-}
{-
solution1 = sat $ do
    y <- sInteger "y"
    x <- sInteger "x"
    w <- sInteger "w"
    dummy <- sInteger "dummy"
    constrain $ (dummy .== 4)
    constrain $ (sNot $ y .<= 1 + x)
    constrain $ (sNot $ 8 .<= w)
    constrain $ (sNot $ dummy .<= 4)
-}
{-
solution2 = sat $ do
    y <- sInteger "y"
    x <- sInteger "x"
    w <- sInteger "w"
    dummy <- sInteger "dummy"
    constrain $ (dummy .== 5)
    constrain $ (sNot $ y .<= 1 + x)
    constrain $ 8 .<= w
    constrain $ (sNot $ dummy .<= 5)
-}
{-
solution3 = sat $ do
    y <- sInteger "y"
    x <- sInteger "x"
    w <- sInteger "w"
    dummy <- sInteger "dummy"
    constrain $ (dummy .== 4)
    constrain $ y .<= 1 + x
    constrain $ (sNot $ 8 .<= w)
    constrain $ (sNot $ dummy .<= 4)
-}

solution4 = sat $ do
    y <- sInteger "y"
    x <- sInteger "x"
    w <- sInteger "w"
    constrain $ y .<= 1 + x
    constrain $ 8 .<= w
    constrain $ (sNot $ (4::SInteger) .<= 5)

solution5 = do
    y <- sInteger "y"
    x <- sInteger "x"
    w <- sInteger "w"
    constrain $ y .<= 1 + x
    constrain $ 8 .<= w
    constrain $ (sNot $ (4::SInteger) .<= 5)



--solution = prove $ \x -> x `shiftL` 2 .== 4 * (x :: SWord8)
someFunc :: IO ()
someFunc =  print "d"
 