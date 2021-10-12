module ImpProgram where
import ImpParser
import Imp (rtZero, deepSimplifyProgram, deepSimplifyRunTime)
import ImpVCGen (bottom, vcGenerator0, fpPWhile, fpWhile)
import ImpIO (completeRoutine)
import Text.Parsec
import Data.Either (fromRight)
-- EJEMPLOS DE EPRESIONES PARSEADAS aexp 
arit_1 = regularParse aexp "(9 + 10 + 11)"
arit_2 = regularParse aexp "9*p0 + 10/6 - 11"
arit_3 = regularParse aexp "9*p - 10/6*x - 11"
arit_4 = regularParse aexp "-11"
arit_5 = regularParse aexp "9*p - 10/6*x - 11*(1 + 3*y) - 3*3"
arit_6 = regularParse aexp "4/6*x + y + 4*(4*(x +y))"

------------------------------{RUNTIMES} ----------------------------------
rtarit_1 = regularParse runtime "9 ++ 10 ++ 11"
rtarit_1' = regularParse runtime "9 + 10 + 11"
rtarit_2 = regularParse runtime "9*p ++ 10/6 -- 11"
rtarit_3 = regularParse runtime "9*p -- 10/6*x -- 11"
rtarit_4 = regularParse runtime "-11"
ind_1 = regularParse runtime "[true]"
ind_2 = regularParse runtime "[a <= x] ++ 33 -- 23"
ind_3 = regularParse runtime "2**[f == 3*x + 5/2] <> (w ++ 1)" --falla
ind_4 = regularParse runtime "2**[f == 3*x + 5/2] ++ w ++ 1"
ind_5 = regularParse runtime "2**([f == 3*x + 5/2] ++ w ++ 1)"
ind_6 = regularParse runtime "[f == 3*x + 5/2] <> x ++ w ++ 1"
ind_7 = regularParse runtime "x ++ w + 1"
ind_8 = regularParse runtime "x ++ (w + 1)"
ind_9 = regularParse runtime "2**[f == 3*x + 5/2] <> x ++ w ++ 1"
ind_10 = regularParse runtime "2**[f == 3*x + 5/2] <> (x + 1) ++ w ++ 1"
ind_11 = regularParse runtime "1 ++ 3**[y>=10]<>(y--10++1)"

--------------------------{PAExp}--------------------------------------
paexp0 = regularParse paexp "1/2*<3*x + 1> + 1/2*<2*y>"

--------------------------{PBExp} -------------------------------------
pbexp0 = regularParse pbexp "<1/2>"
--------------------------{PROGRAMAS DETERMINISTICOS SIN BUBLES } ----------------------------------
p1_1 = "x:=10; y:=3"
p1_2 = "x:=10; y:=3; it(x>=y){skip; skip}"
p1_3 = "x:=10; y:=3; if(x>=y){skip; skip} else{empty}"
p1_4 = "x:=10; y:=3; if(x>=y){skip; skip} else{if(true){z:=3/5; w:=3}else{skip; empty}}"
p1_5 = "if(x>=y){skip; skip} else{if(true){z:=3/5; w:=3} else{skip; empty}}"
p1_6 = "x:=x-1; if(x>=y){skip; y:= 2*x} else{if(w>=8){w:= 3; x:=w+x} else{y:=5}}"
--------------------------{PROGRAMAS DETERMINISTICOS CON BUBLES} ------------------------------------
p2_1 = "while(x > 0){inv = 1 ++ 2**[x>0]<>x }{x:= x-1}"
p2_2 = "x:= 10; while(x > 0){inv = 1 ++ 2**[x>0]<>x }{x:= x-1}"
p2_3 = "while(y >= 10){inv = 1 ++ 3**([y>=10]<>(y--10++1))}{y:=y-1;x:=x+1}"

---------------------------{PROGRAMAS PROBABILÍSTICOS SIN BUCLES}------------------------------------
p3_1 = "succ:~ 1/2* <3*x+ 1> + 1/2* <2*y>"
p3_2 = "pif(<1/2>){succ:~ 5/100* <0> + 95/100* <1>} pelse {pif(<1/2>) {succ:~  5/100* <0> + 95/100* <1>} pelse{succ:~ 95/100* <0> + 5/100* <1>}}"
p3_3 = "pit(<9/10>){ if(x > 10){skip} else{ x:= x-1}}"
----------------------------{PROGRAMAS PROBABILÍSTICOS CON BUCLES}------------------------------------
p4_1 = "pwhile(<1/2>){pinv = 3}{skip}"
p4_2 = "while(c == 1){inv = 1 ++ 4**[c == 1]}{c:~ 1/2* <0> + 1/2* <1>}"
p4_3 = "pwhile(<9/10>) {pinv = 1 ++ 10**(1 ++ 4**[c == 1])}{while(c == 1){inv = 1 ++ 4**[c == 1]} {c:~ 1/2* <0> + 1/2* <1>}}"
p4_4 = "for(3){pwhile(<1/2>){pinv = 3}{skip}}"
-----------------------------{DISTRIBUCIONES PROBABILÍSTICAS}------------------------------------------------------------------
p5_1 = "y :~ <12> "
p5_2 = "coin :~ coin_flip(2/4)"
p5_3 = "euler:~ uniform(-4, 7)"
p5_4 = "pi:~ uniform(10)"
p5_5 = "v:~ uniform(1)"
p5_6 = "v:~ uniform(-1, -4)"
------------------------------{MÉTODOS PARA UNIR LOS PROCESOS}-----------------------------------------------------------------

run' :: String -> IO ()
run' input = case parseProgram "<interactive>" input of
  Left err  -> print err
  Right program -> let runt = fst $ vcGenerator0 $ deepSimplifyProgram program in
                   print $ (show runt) ++ " ===> " ++ (show (deepSimplifyRunTime runt))

run :: String -> IO ()
run input = case parseProgram "<interactive>" input of
  Left err  -> print err
  Right program -> completeRoutine (deepSimplifyProgram program) input rtZero

run'' :: String -> IO ()
run'' input = case parseProgram "<interactive>" input of
  Left err  -> print err
  Right program -> print (deepSimplifyProgram program) 
{-

fpd :: String -> String -> String -> String -> Int ->IO ()
fpd x b p runt n = do
  let runt' = fromRight $ parseRunTime "<interactive>" runt
  let b'    = fromRight $ parseBExp "<interactive>" b
  let p'    = fromRight $ parseProgram "<interactive>" p
  let x'    = fromRight $ parseRunTime "<interactive>" x
  print $ fpWhile x' b' p' runt' n

fpp :: String -> String -> String -> String -> Int ->IO ()
fpp x pb p runt n = do
  let runt' = fromRight $ parseRunTime "<interactive>" runt
  let pb'    = fromRight $ parsePBExp "<interactive>" pb
  let p'    = fromRight $ parseProgram "<interactive>" p
  let x'    = fromRight $ parseRunTime "<interactive>" x
  print $ fpPWhile x' pb' p' runt' n

  -}