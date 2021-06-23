import           Data.String (IsString (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)
import Data.List
type Name = String
---------------------------------------- (Funciones útiles)---------------------------------------
-- rmdups toma una lista de elementos de tipo a y retorna una lista sin duplicados
rmdups :: (Eq a) =>[a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)
---------------------------------------- (Funciones útiles)---------------------------------------



---------------------------------------- (Expreciones Aritméticas)--------------------------------
-- Definición de estructuras aritméticas
data AExp = Lit Float -- Números
          | Var Name -- Varibles x, y, z
          | AExp :+: AExp -- suma de expresiones aritméticas
          | Float :*: AExp deriving (Eq) -- pondelación por una constante 

-- Definición del método show para AExp
instance Show AExp where 
  show (Lit n) = show n
  show (Var x) = show x
  show (e_1 :+: e_2) = show e_1 ++" + "++ show e_2
  show (e_1 :*: e_2) = show e_1 ++" * "++ show e_2       

-- sustAExp toma un Name "x", un AExp aritFor y un AExp aritIn
-- sustituye todas las instancias "x" en AritIn y por aritFor
sustAExp :: Name -> AExp -> AExp -> AExp
sustAExp _  _ (Lit n) = (Lit n)
sustAExp x  aritFor (Var y) = if (x == y) then aritFor else (Var y)
sustAExp x aritFor (e_1 :+: e_2) = (sustAExp x aritFor e_1) :+: (sustAExp x aritFor e_2)
sustAExp x aritFor (k :*: e) = k :*: (sustAExp x aritFor e)

-- freeVars toma un AExp arit y retorna una lista de todas las variables libres
-- considerando que un número está asociado a la variable vacía "".
freeVars :: AExp -> [String]
freeVars arit = sort (rmdups (fvar arit)) where
  fvar (Lit _ ) = [""]
  fvar (Var x)  = [x]
  fvar (e_1 :+: e_2) = (fvar e_1) ++ (fvar e_2)
  fvar (_ :*: e) = (fvar e)

-- weightVar toma un Aexp arit y una variable var, retorna el peso suma de todas las instancias de esa variable var
-- en el AExp arit.
weightVar :: AExp -> String-> Float
weightVar (Lit n) var | var =="" = n  
                      | otherwise = 0.0
weightVar (Var x) var | var == x = 1
                      | otherwise = 0.0
weightVar (e_1 :+: e_2) var = (weightVar e_1 var) + (weightVar e_2 var)
weightVar (k :*: e) var = k * (weightVar e var)
---------------------------------------- (Expresiones Aritméticas)--------------------------------



----------------------------------( Simplificador Expresiones Aritméticas )-----------------------

normArit :: AExp -> AExp
normArit arit = foldr f (Lit 0.0) wvars where
  f = \x y -> x:+:y
  vars = freeVars arit -- variables libres del programa
  weights = map (weightVar arit) vars -- todos los pesos de las diferentes variables
  wvars = map (\(k, x) ->  k:*:(Var x)) (zip weights vars) -- arreglo de pares (peso, variable)


-- simplifyArit toma un AExp arit y retorna una versión simplificada             
simplifyArit::AExp -> AExp
simplifyArit ((Lit 0.0) :+: arit) = simplifyArit arit
simplifyArit (arit :+: (Lit 0.0)) = simplifyArit arit
simplifyArit (arit_1 :+: arit_2) = (simplifyArit arit_1) :+: (simplifyArit arit_2)
simplifyArit (1.0 :*: arit) = simplifyArit arit
simplifyArit (k :*: arit) = k :*: (simplifyArit arit)
simplifyArit otherwise = otherwise 

completeNormArit :: AExp -> AExp
completeNormArit = simplifyArit.normArit

----------------------------------( Simplificador Expresiones Aritméticas)-------------------------



----------------------------------(Expresiones Booleanas)------------------------------------------

--def de expresiones Boolenas
data BExp = True' -- Constante True
          | False' -- Constante False
          | AExp :<=: AExp -- mayor igual entre expresiones aritméticas
          | AExp :==: AExp -- igualdad expresiones aritméticas
          | BExp :|: BExp -- Or lógico
          | BExp :&: BExp -- And Lógico
          | Not BExp deriving (Show, Eq) -- Negación expresión booleana

-- función de sustitución toma una variable "x", un AExp aritFor y una expresión booleana AritIn
-- reemplaza todas las indicendias de "x" en la expresión aritIn por la expresión aritFor.
sustBExp :: Name -> AExp -> BExp -> BExp
sustBExp _  _ True' = True'
sustBExp _  _ False' = False'
sustBExp x aritFor (e_1 :<=: e_2) = (sustAExp x aritFor e_1) :<=: (sustAExp x aritFor e_2)
sustBExp x aritFor (e_1 :==: e_2) = (sustAExp x aritFor e_1) :==: (sustAExp x aritFor e_2)
sustBExp x aritFor (e_1 :|: e_2) = (sustBExp x aritFor e_1) :|: (sustBExp x aritFor e_2)
sustBExp x aritFor (e_1 :&: e_2) = (sustBExp x aritFor e_1) :&: (sustBExp x aritFor e_2)
sustBExp x aritFor (Not e) = (Not (sustBExp x aritFor e))

----------------------------------(Expresiones Booleanas)------------------------------------------



----------------------------------( Runtimes )-----------------------------------------------------

-- def de Runtimes
data RunTime = RunTimeArit AExp -- runtime hecho a partir de una expresión aritmética
             | BExp :<>: RunTime -- multiplicación por una condición
             | RunTime :++: RunTime -- suma de runtime
             | Float :**: RunTime  deriving (Eq) -- ponderación por constante 

-- def de método show para la clase
instance Show RunTime where 
  show (RunTimeArit arit) = show arit
  show (e_b :<>: (RunTimeArit (Lit 1))) = "["++ (show e_b) ++ "]"
  show (e_b :<>: (RunTimeArit (Lit n))) = "["++ (show e_b) ++ "]*" ++ (show n)
  show (e_b :<>: runt) = "["++ (show e_b) ++ "]*" ++ "(" ++ (show runt) ++ ")"
  show (e_1 :++: e_2) =  show e_1 ++" + "++ show e_2    
  show (e_1 :**: e_2) = show e_1 ++" * " ++ show e_2        

-- función de sustitución toma una variable "x", un AExp aritFor, un RunTime runtIn
-- reemplaza todas las indicendias de "x" en la expresión runtIn por la expresión aritFor.
sustRuntime :: Name -> AExp -> RunTime -> RunTime
sustRuntime x aritFor (RunTimeArit aritIn) = (RunTimeArit (sustAExp x aritFor aritIn))
sustRuntime x aritFor (e_b :<>: e_r) = (sustBExp x aritFor e_b) :<>: (sustRuntime x aritFor e_r)
sustRuntime x aritFor (e_1 :++: e_2) = (sustRuntime x aritFor e_1) :++: (sustRuntime x aritFor e_2)
sustRuntime x aritFor (k :**: e) = k :**: (sustRuntime x aritFor e)


----------------------------------( Runtimes )-----------------------------------------------------


----------------------------------(Programas )-----------------------------------------------------
data Program = Skip
            | Empty
            | Set Name AExp
            | Seq Program Program
            | If BExp Program Program
            | While BExp Program RunTime deriving(Show, Eq)
----------------------------------( Programas )-----------------------------------------------------


----------------------------------( Restriction )-----------------------------------------------------

data Restriction = RunTime :!==: RunTime -- Restriction de igualdad entre RunTime
               | RunTime :!<=: RunTime deriving(Show, Eq) -- Restriction de mayor igual entre RunTime

-- def de un función de fold para la estructura Restriction
foldrRes :: (a -> a -> b) -> (RunTime -> a) -> Restriction -> b
foldrRes f g (e_1 :!==: e_2) = f (g e_1) (g e_2)
foldrRes f g (e_1 :!<=: e_2) = f (g e_1) (g e_2)

-- def de la función map para la estructura REstriction
mapRes :: (RunTime -> RunTime) -> Restriction -> Restriction
mapRes f (e_1 :!==: e_2) = (f e_1) :!==: (f e_2)
mapRes f (e_1 :!<=: e_2) = (f e_1) :!<=: (f e_2)

----------------------------------( Restriction )-----------------------------------------------------



----------------------------------(vc gen)-------------------------------------------------------------

vcGenerator ::  Program -> RunTime -> (RunTime, [Restriction])
vcGenerator Skip runt = ((RunTimeArit (Lit 1.0)) :++: runt, [])
vcGenerator Empty runt = (runt, [])
vcGenerator (Set x arit ) runt = ((RunTimeArit (Lit 1.0)) :++: (sustRuntime x arit runt), [])
vcGenerator (If e_b e_t e_f) runt = ( (RunTimeArit (Lit 1.0)) :++:((e_b :<>:fst vc_t):++:((Not e_b):<>: fst vc_f)),  (snd vc_t) ++ (snd vc_f)) where 
    vc_t = vcGenerator e_t runt
    vc_f = vcGenerator e_f runt
vcGenerator (Seq p_1 p_2) runt = (fst vc_1, (snd vc_1) ++ (snd vc_2))   where
    vc_2 = vcGenerator p_2 runt
    vc_1 = vcGenerator p_1 (fst vc_2)
vcGenerator (While e_b p inv) runt = (inv, [l_inv :!<=: inv] ++ (snd vc_p)) where
    vc_p = vcGenerator p inv
    l_inv = (RunTimeArit (Lit 1.0) :++: ( ((Not e_b) :<>: runt):++: (e_b :<>: (fst vc_p))))

---------------------------(vc gen)---------------------------------------------------------

---------------------------(Simplificar)---------------------------------------------------------

simplifyBExp :: BExp -> BExp
simplifyBExp (True' :|: _ ) = True'
simplifyBExp ( _ :|: True' ) = True'
simplifyBExp ( e_b :|: False' ) = e_b
simplifyBExp (False' :|: e_b) = e_b
simplifyBExp (False' :&: _ ) = False'
simplifyBExp ( _ :&: False') = False'
simplifyBExp (True' :&: e_b) = e_b
simplifyBExp (e_b :&: True') = e_b
simplifyBExp (Not(Not e_b)) = e_b
simplifyBExp otherwise = otherwise

deepSimplifyBExp :: BExp -> BExp
deepSimplifyBExp True' = True'
deepSimplifyBExp False' = False'
deepSimplifyBExp (e_1 :<=: e_2) = e_1 :<=: e_2
deepSimplifyBExp (e_1 :==: e_2) = e_1 :==: e_2
deepSimplifyBExp (e_1 :|: e_2) = simplifyBExp ((deepSimplifyBExp e_1) :|: (deepSimplifyBExp e_2))
deepSimplifyBExp (e_1 :&: e_2) = simplifyBExp ((deepSimplifyBExp e_1) :&: (deepSimplifyBExp e_2))
deepSimplifyBExp (Not e_b) = simplifyBExp (Not (deepSimplifyBExp e_b))


simplifyRuntime :: RunTime -> RunTime
simplifyRuntime ((RunTimeArit (Lit n)) :++: ((RunTimeArit (Lit m))) :++: runt) = (RunTimeArit (Lit (n + m))) :++: runt
simplifyRuntime (True' :<>: runt) = runt
simplifyRuntime (False' :<>: _ )  = (RunTimeArit (Lit 0.0))
simplifyRuntime ( _ :<>: (RunTimeArit (Lit 0.0))) = RunTimeArit (Lit 0.0)
simplifyRuntime ((RunTimeArit (Lit 0.0)) :++: runt) = runt
simplifyRuntime (runt :++: (RunTimeArit (Lit 0.0))) = runt 
simplifyRuntime ((RunTimeArit (Lit n)) :++: (RunTimeArit (Lit m))) = (RunTimeArit (Lit (n + m)))
simplifyRuntime ( m :**: (RunTimeArit (Lit n))) = (RunTimeArit (Lit (m * n)))
simplifyRuntime ( 1.0 :**: runt) = runt
simplifyRuntime ( 0.0 :**: _ ) = RunTimeArit (Lit 0.0)
simplifyRuntime otherwise = otherwise

deepSimplifyRuntime :: RunTime -> RunTime
deepSimplifyRuntime (RunTimeArit arit) = (RunTimeArit arit)
deepSimplifyRuntime (bexp :<>: runt) = simplifyRuntime ((deepSimplifyBExp bexp) :<>: (deepSimplifyRuntime runt))
deepSimplifyRuntime (e_1 :++: e_2) = simplifyRuntime ((deepSimplifyRuntime e_1) :++: (deepSimplifyRuntime e_2))
deepSimplifyRuntime (k :**: runt) = simplifyRuntime (k :**: (deepSimplifyRuntime runt))


---------------------------(Simplificar)---------------------------------------------------------

---------------------------(Preparación para z3)-------------------------------------------------

type Context = [BExp]

-- findcondition retorna todas las instancias de BExp dentro un Runtime
findConditionRuntime :: RunTime -> Context
findConditionRuntime (RunTimeArit _) = []
findConditionRuntime ((Not bexp) :<>: runt) = [bexp] ++ (findConditionRuntime runt)
findConditionRuntime (bexp :<>: runt) = [bexp] ++ (findConditionRuntime runt)
findConditionRuntime (e_1 :++: e_2) = (findConditionRuntime e_1) ++ (findConditionRuntime e_2)
findConditionRuntime (_ :**: runt) = (findConditionRuntime runt)

-- bools retorna una matriz con todas las posibles combinaciones False/True 
-- de tamaño n. 
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) r ++ map (True:) r where
  r = bools (n-1)


allConditions :: Restriction -> [[(BExp, Bool)]]
allConditions rest = map (zip conds) vals where
      conds = rmdups (foldrRes (++) (findConditionRuntime) rest) 
      vals = bools (length conds)

reduceContext :: (BExp, Bool) -> BExp
reduceContext (bexp, True) = bexp
reduceContext (bexp, False) = Not(bexp)

allContext :: Restriction -> [Context]
allContext rest = map (map reduceContext) (allConditions rest)

evalCondition :: BExp -> RunTime -> RunTime
evalCondition bexp (RunTimeArit arit) = (RunTimeArit arit)
evalCondition bexp1 (bexp2 :<>: runt) | bexp1 == bexp2 = (evalCondition bexp1 runt)
                                      | (deepSimplifyBExp (Not bexp1)) == bexp2 = (RunTimeArit (Lit 0))
                                      | otherwise = (bexp2 :<>: (evalCondition bexp1 runt))
evalCondition bexp (e_1 :++: e_2) = (evalCondition bexp e_1) :++: (evalCondition bexp e_2)
evalCondition bexp (k :**: runt) = k :**: (evalCondition bexp runt)

evalAllCondition :: RunTime-> Context -> RunTime
evalAllCondition runt []= runt
evalAllCondition runt (b:bs)= (evalAllCondition (evalCondition b runt) bs)

evalRestriction :: Restriction->Context-> Restriction
evalRestriction (e_1 :!<=: e_2) bs = (evalAllCondition e_1 bs) :!<=: (evalAllCondition e_2 bs)
evalRestriction (e_1 :!==: e_2) bs = (evalAllCondition e_1 bs) :!==: (evalAllCondition e_2 bs) 

{--
restrictionsToZ3 :: Restriction -> ([Context], [Restriction])
restrictionsToZ3 rest = (cs, rs) where 
  sres = restrictionMap (deepSimplifyRuntime) rest
  cs = allContext sres
  rs = map (evalRestriction sres) cs
-}
----------------------------(Preparación para z3)---------------------------------------------------------

---------------------------(Programas de ejemplo)---------------------------------------------------------

-- programa de ejemplo 

p0 = (While False' Empty (RunTimeArit (Lit 5)))
vc_0 = vcGenerator p0 (RunTimeArit (Lit 0)) 
r0 = fst vc_0
s0 = deepSimplifyRuntime (fst vc_0)

-- programa 1 de ejemplo

l0 = Set  "x" ((Var "x"):+:(Lit 1)) --cambiar por menos
l3 = Set "y" (2:*:(Var "x"))
l6 = Set "w" (Lit 3)
l7 = Set "x" ((Var  "w") :+: (Var "x"))
l9 = Set "y" (Lit 5)
l5_9 = If ((Lit 8) :<=: (Var "w")) (Seq l6 l7) l9
l1_9 = If ((Var "y") :<=: (Var "x")) (Seq Skip l3) l5_9
l0_9 = Seq l0 l1_9

vc_1 = vcGenerator l0_9 (RunTimeArit (Lit 0.0))
s1 = deepSimplifyRuntime (fst vc_1)


---------------------------(Restricciones de ejemplo)---------------------------------------------------------
{-
c1 = (2 :*:(Var "x")):<=:(Var "y")
c2 = (Var "x"):<=:(Lit 0)
c3 = (Var "x"):<=:(Lit 0)
c4 = (Var "ww"):<=:(Lit 0)

arit1 = (Var "x") :+: (Var "y")
arit2 = (Var "y") :+: (Lit 1)

runt1 = c1 :<>: (RunTimeArit arit1)
runt2 = c2 :<>: (RunTimeArit arit2)

restriction = runt1 :!<=: runt2

conds = findCo restriction

vals = bools (length conds)

allv = map (zip conds) vals
allc = allConditions restriction

allcontext = allContext restriction
z3Input = restrictionsToZ3 restriction

expresionBoolean = (Not (Not (Not True')))
sexp = deepSimplifyBExp expresionBoolean

sumando1 = (Not( (Lit 8.0 ):<=: Var "w")):<>: (RunTimeArit(Lit 4.0)) 
sumando2 = ( (Lit 8.0):<=: Var "w"):<>: (RunTimeArit(Lit 5.0))

runtr = sumando1 :++: sumando2

res = s1 :!<=: runtr

ejemploP3 = restrictionsToZ3 res
-}
-------------------------------(Simplificaciónes Aritméticas)--------------------------------------------------------

a1 = (((((Var "x") :+: (Var "x")) :+: (Lit 1)) :+: ((-2) :*: (Var "y"))) :+: (Lit 8.0))

array_a1 = simplifyArit.normArit $ a1
--array_a1 = normArit $ a1
-------------------------------(Simplificaciónes Aritméticas)--------------------------------------------------------