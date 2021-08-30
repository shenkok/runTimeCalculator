import Imp
import ImpToSBV
import ImpIO


testArit :: IO ()
testArit = do
    -- Test de expresiones Aritméticas
    let arit_1 = Lit 3 :+: Lit 8
    let test_1 = Lit 11
    let arit_2 = 2:*: (Lit 10 :+: Var "x")
    let test_2 = (2.0:*: Var "x") :+: Lit 20.0
    putStrLn "Inicio de Test's"
    putStrLn $ if completeNormArit arit_1 == test_1  then "OK" else "FAIL!"
    putStrLn $ show (completeNormArit arit_2) 
    putStrLn $ show test_2
main :: IO ()
main = testArit
