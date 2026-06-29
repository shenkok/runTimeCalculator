{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeAbstractions    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ImpSBV
import Imp
import ImpIO
import ImpVCGen
import ImpParser
import ImpProgram
import Data.SBV
import Data.SBV.Internals (AlgReal, SolverContext (internalVariable))
import Data.Either (fromRight)
------------------------------{MÉTODO PARA UNIR LOS PROCESOS}-----------------------------------------------------------------

run :: String -> IO ()
run input = case parseProgram "<interactive>" input of
  Left err  -> print err
  Right program -> completeRoutine (deepSimplifyProgram program) input rtZero

-- |Calcula la iteración de punto fijo de orden n de una transformada de un while determinista

fp :: String -> String -> String -> String -> Int ->IO ()
fp x b p runt n = case (parseRunTime "<interactive>" x, parseBExp "<interactive>" b, parseProgram "<interactive>" p, parseRunTime "<interactive>" runt) of
  (Right x', Right b',Right p',Right runt') -> print $ deepSimplifyRunTime $ fpWhile x' b' p' runt' n
  (_, _,  _, _) -> error "Ha ocurrido un error"

-- Ejemplo iteración número 3, para el programa while(x==0){x:x-1}, con respecto al tiempo de ejecución 0 y empezando desde 0
exampleFp :: IO ()
exampleFp = fp "0" "x==0" "x:=x-1" "0" 3
examplePresentacion = fp "0" "c==1" "c :~ 1/2* <0> + 1/2* <1>" "0"

p5S_6  = "while(c == 1){inv = 1 ++ 4**[c == 1]}{ c :~ 1/2* <0> + 1/2* <1>}"

-- | Calcula la iteración de punto fijo de orden n de una transformada de un while probabilista
fpp :: String -> String -> String -> String -> Int ->IO ()
fpp x pb p runt n = case (parseRunTime "<interactive>" x, parsePBExp "<interactive>" pb, parseProgram "<interactive>" p, parseRunTime "<interactive>" runt) of
  (Right x', Right pb',Right p',Right runt') ->  print $ deepSimplifyRunTime $ fpPWhile x' pb' p' runt' n
  (_, _,  _, _) -> error "Ha ocurrido un error"

-- Ejemplo iteración número 5, para el programa pwhile (<1/2>) {skip} con respecto al tiempo de ejecución 3 y empezando desde 0


 -- {EJEMPLOS DE PROGRAMAS Y SUS RESPECTIVAS ESTRUCTURAS} --

programaPaper :: String
programaPaper = "while(c == 1){inv = 1 ++ 4**[c == 1]}{c:~ 1/2* <0> + 1/2* <1>}"

main :: IO ()
main = do
  result <- sat $ do
    a <- sRational "a"   -- existencial normal, aparece en el modelo
    constrain $ quantifiedBool $
      \(Forall @"c" (c :: SRational)) ->
            ((c .== 1) .=> ((4 :: SRational) - a .<= 0))
        .&& ((c ./= 1) .=> sTrue)
  print result

problem_c13_one_unknown :: IO ()
problem_c13_one_unknown = do
  result <- sat $ do
    a <- sReal "a"
    constrain $ quantifiedBool $
      \(Forall @"x" (x :: SReal))
       (Forall @"y" (y :: SReal))
       (Forall @"z" (z :: SReal)) ->
        let b = 4 :: SReal
            c = 1 :: SReal
            p = (y .<= x) .&& (x .<= z)
            q = (y .<= x + 0.5) .&& (x + 0.5 .<= z)
        in     ((p .&& q)           .=> (b .>= 4))
           .&& ((sNot p .&& q)      .=> (1 .<= a))
           .&& ((p .&& sNot q)      .=> (2 .<= b * (z - x + c)))
           .&& ((sNot p .&& sNot q) .=> (1 .<= a))

  print result
  case getModelValue "a" result of
    Just v  -> putStrLn $ "a = " ++ show (v :: AlgReal)
    Nothing -> putStrLn "No encontrado"

problem_7 :: IO ()
problem_7 = do
  result <- sat $ do
    a <- sReal "a"
    c <- sReal "c"

    constrain $ quantifiedBool $
      \(Forall @"x" (x :: SReal)) ->
        let p = x .> 0
            q = x - 1 .> 0
        in
          -- Caso 1
          ((p .&& q) .=> ((2 :: SReal) .<= 2))
          .&&
          -- Caso 2
          ((sNot p .&& q)      .=> (1 .<= a))
          .&&
          -- Caso 3
          ((p .&& sNot q)      .=> (2 .<= 2 * (x + c)))
          .&&
          -- Caso 4
          ((sNot p .&& sNot q) .=> (1 .<= a))

  print result



-- {PRUEBA DE SOLVER INPUT PARA VARIBLES EXISTENCIALES Y UNIVERSALES} --
condicion_1 :: BExp
condicion_1 = fromRight (error "parse error") (regularParse bexp "x > 0")

condicion_2 :: BExp
condicion_2 = fromRight (error "parse error") (regularParse bexp "x - 1> 0")

conclusion_1 :: BExp
conclusion_1 = fromRight (error "parse error") (regularParse bexp "2 <=2")

conclusion_3 :: BExp
conclusion_3 = fromRight (error "parse error") (regularParse bexp "2<=2*(x+c)")

conclusion_2 ::BExp
conclusion_2 = fromRight (error "parse error") (regularParse bexp "1<=a")

conclusion_4 :: BExp
conclusion_4 = fromRight (error "parse error") (regularParse bexp "1<=a")

implica_1 = Implication 
              { hypothesis = [condicion_1, condicion_2]        -- p ∧ q
              , conclusion = conclusion_1 }                     -- 2 ≤ 2

implica_2 = Implication 
              { hypothesis = [Not condicion_1, condicion_2]    -- ¬p ∧ q
              , conclusion = conclusion_2 }                     -- 1 ≤ a

implica_3 = Implication 
              { hypothesis = [condicion_1, Not condicion_2]    -- p ∧ ¬q
              , conclusion = conclusion_3 }                     -- 2 ≤ 2*(x+c)

implica_4 = Implication 
              { hypothesis = [Not condicion_1, Not condicion_2] -- ¬p ∧ ¬q
              , conclusion = conclusion_4 }                      -- 1 ≤ a

exampleInput :: SolverInput'
exampleInput = SolverInput'
  { existential = ["a", "c"]
  , for_all = ["x"]
  , solver_formulaes = [implica_1, implica_2, implica_3, implica_4]
  }

-- {PRUEBA DE SOLVER INPUT PARA VARIBLES EXISTENCIALES } --

condicionA :: BExp
condicionA = fromRight (error "parse error") (regularParse bexp "x >= 0")

condicionB :: BExp
condicionB = fromRight (error "parse error") (regularParse bexp "x < 0")

conclusionA :: BExp
conclusionA = fromRight (error "parse error") (regularParse bexp "1 < x")

conclusionB :: BExp
conclusionB = fromRight (error "parse error") (regularParse bexp "1 < 0")

implicaA :: Implication
implicaA = Implication 
              { hypothesis = [condicionA]
              , conclusion = conclusionA
              }

implicaB :: Implication
implicaB = Implication 
              { hypothesis = [condicionA]
              , conclusion = conclusionA
              }


exampleInput2 :: SolverInput'
exampleInput2 = SolverInput'
  { existential = []
  , for_all = ["x"]
  , solver_formulaes = [implicaA, implicaB]
  }


--{ 3 VARIABLES}--
-- Condiciones
cond_p :: BExp
cond_p = fromRight (error "parse error") (regularParse bexp "y <= x && x <= z")

cond_q :: BExp
cond_q = fromRight (error "parse error") (regularParse bexp "y <= x + 0.5 && x + 0.5 <= z")

-- Conclusiones
concl_b_geq_4 :: BExp
concl_b_geq_4 = fromRight (error "parse error") (regularParse bexp "4 >= 4")

concl_a_geq_1 :: BExp
concl_a_geq_1 = fromRight (error "parse error") (regularParse bexp "1 <= a")

concl_intervalo :: BExp
concl_intervalo = fromRight (error "parse error") (regularParse bexp "2 <= 4 * (z - x + 1)")

-- Implicaciones
impl_c13_pq :: Implication
impl_c13_pq = Implication
                { hypothesis = [cond_p, cond_q]
                , conclusion = concl_b_geq_4 }

impl_c13_npq :: Implication
impl_c13_npq = Implication
                { hypothesis = [Not cond_p, cond_q]
                , conclusion = concl_a_geq_1 }

impl_c13_pnq :: Implication
impl_c13_pnq = Implication
                { hypothesis = [cond_p, Not cond_q]
                , conclusion = concl_intervalo }

impl_c13_npnq :: Implication
impl_c13_npnq = Implication
                 { hypothesis = [Not cond_p, Not cond_q]
                 , conclusion = concl_a_geq_1 }

-- SolverInput
c13Input :: SolverInput'
c13Input = SolverInput'
  { existential     = ["a"]
  , for_all          = ["x", "y", "z"]
  , solver_formulaes = [impl_c13_pq, impl_c13_npq, impl_c13_pnq, impl_c13_npnq]
  }

-- { EJEMPLOS DE  RunTime }--
runtime1 :: RunTime
runtime1 = fromRight (error "parse error") (regularParse runtime " 1 ++ [x >= 0] ++ [w < 0]")

runtime2 :: RunTime
runtime2 = fromRight (error "parse error") (regularParse runtime " 2 ++ [u >= 2] ++ [y < 0]")

-- {EJEMPLOS DE RRunTime} --

restrictionA :: RRunTime
restrictionA = runtime1 :!<=: rtOne