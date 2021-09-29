module ImpProgram where
import ImpParser
import Imp
import Text.Parsec
-- EJEMPLOS DE EPRESIONES PARSEADAS aexp 
arit_1 = regularParse aexp "(9 + 10 + 11)"
arit_2 = regularParse aexp "9*p0 + 10/6 - 11"
arit_3 = regularParse aexp "9*p - 10/6*x - 11"
arit_4 = regularParse aexp "-11"
arit_5 = regularParse aexp "9*p - 10/6*x - 11*(1 + 3*y) - 3*3"
arit_6 = regularParse aexp "4/6*x + y + 4*(4*(x +y))"

rtarit_1 = regularParse runtime "9 ++ 10 ++ 11"
rtarit_1' = regularParse runtime "9 + 10 + 11"
rtarit_2 = regularParse runtime "9*p ++ 10/6 -- 11"
rtarit_3 = regularParse runtime "9*p -- 10/6*x -- 11"
rtarit_4 = regularParse runtime "-11"
ind_1 = regularParse runtime "[true]"
ind_2 = regularParse runtime "[a <= x] ++ 33 -- 23"
ind_3 = regularParse runtime "2**[f == 3*x + 5/2] <> (w ++ 1)"
ind_4 = regularParse runtime "2**[f == 3*x + 5/2] ++ w ++ 1"
ind_5 = regularParse runtime "2**([f == 3*x + 5/2] ++ w ++ 1)"
ind_6 = regularParse runtime "[f == 3*x + 5/2] <> x ++ w ++ 1"
ind_7 = regularParse runtime "x ++ w + 1"
ind_8 = regularParse runtime "x ++ (w + 1)"
ind_9 = regularParse runtime "2**[f == 3*x + 5/2] <> x ++ w ++ 1"
ind_10 = regularParse runtime "2**[f == 3*x + 5/2] <> (x + 1) ++ w ++ 1"

-- EJEMPLOS DE PROGRAMAS
programa0 :: Either ParseError Program
programa0 = regularParse program "if (true) { a := 11; } else { a := 12; };"

