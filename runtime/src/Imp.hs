module Imp where

import Data.List
type Name = String
type Constant = Float
type Names = [Name]
---------------------------------------- { FUNCIONES ÚTILES }---------------------------------------
-- | Elimina los elementos repetidos en una lista
rmdups :: (Eq a) =>[a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

-- | bools retorna una matriz con todas las posibles combinaciones False/True 
-- de tamaño n. 
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) r ++ map (True:) r where
  r = bools (n-1)



---------------------------------------- { EXPRESIONES ARITMÉTICAS }--------------------------------
-- | Definición de Expresiones Aritméticas
-- NOTA      : Para que el solver pueda manejar varibles Reales, Enteras o mixtas
--             se podría agregar como input el tipo a Var.
--             data = Var Name Tipo
-- NOTA       : He pensado bastante el usar sólo expresiones lineales y si se podría generalizar un poco más.
--            En la página oficial de SBV link: https://hackage.haskell.org/package/sbv-8.16/docs/Data-SBV.html#g:12
--            mencionan que
--            SBV can deal with real numbers just fine, since the theory of reals is decidable.
--            (See http://smtlib.cs.uiowa.edu/theories-Reals.shtml.)
--            In addition, by leveraging backend solver capabilities,
--            SBV can also represent and solve non-linear equations involving real-variables.
--           (For instance, the Z3 SMT solver, supports polynomial constraints on reals starting with v4.0.)
--           Seguramente deba investigar en mayor profundidad los límites de Z3, pensando en usar polinomios de mayor grado

data AExp = Lit Constant                    -- Números
          | Var Name                        -- Varibles x, y, z
          | AExp :+: AExp                   -- Suma de expresiones aritméticas
          | Constant :*: AExp deriving (Eq) -- Ponderación por una constante 

---------------------------------------- { FUNCIONES EXPRESIONES ARITMÉTICAS }--------------------------------
-- | Definición del método show para AExp
instance Show AExp where 
  show (Lit n) = show n
  show (Var x) = show x
  show (e_1 :+: e_2) = show e_1 ++" + "++ show e_2
  show (e_1 :*: e_2) = show e_1 ++" * "++ show e_2       


-- | Sustituye todas las instancias "x" en AritIn y por aritFor
sustAExp :: Name -> AExp -> AExp -> AExp
sustAExp _  _ (Lit n) = (Lit n)
sustAExp x  aritFor (Var y) = if (x == y) then aritFor else (Var y)
sustAExp x aritFor (e_1 :+: e_2) = (sustAExp x aritFor e_1) :+: (sustAExp x aritFor e_2)
sustAExp x aritFor (k :*: e) = k :*: (sustAExp x aritFor e)

-- | Toma un AExp arit y retorna una lista de todas las variables libres
-- considerando que un número está asociado a la variable vacía "".
freeVars :: AExp -> Names
freeVars arit = sort (rmdups (fvar arit)) where
  fvar (Lit _ ) = [""]
  fvar (Var x)  = [x]
  fvar (e_1 :+: e_2) = (fvar e_1) ++ (fvar e_2)
  fvar (_ :*: e) = (fvar e)

-- | WeightVar toma un Aexp arit y una variable var, retorna el peso suma de todas las instancias de esa variable var
-- en el AExp arit, se considera n:*:"" como equivalente a (Lit n) .
weightVar :: AExp -> Name -> Constant
weightVar (Lit n) var | var =="" = n  
                      | otherwise = 0
weightVar (Var x) var | var == x = 1
                      | otherwise = 0
weightVar (e_1 :+: e_2) var = (weightVar e_1 var) + (weightVar e_2 var)
weightVar (k :*: e) var = k * (weightVar e var)

---------------------------------- { SIMPLIFICAR Y MORMALIZAR EXPRESIONES ARITMÉTICAS } -----------------------
-- | NOTA : El algoritmo puede ser mas elegante, pensaba en definir por una función de tome un polimio normalizado, un monomio
-- y retorne un polinomio normalizado. En base a eso se podría eliminar el considerar a  n:*:"" como equivalente a (Lit n).
-- En general creo que se podría implementar algo parecido a la tarea 1 de lenguajes 2019-2, Tarea de polinomios.
---------------------------------------------------------------------------------------------------------------
-- | Descripción del algoritmo 
-- 1. Extraer las variables libres de la expresión
-- 2. Calcular el peso asociado a cada una de las variables
-- 3. Definir una función auxiliar que un par (peso, variable) y retorma un monomio peso:*:variable
-- 4. Entregar un arreglo con los monomios respectivos.
-- 5. Correr un fold, con caso base (Lit 0) y usando la suma de polinomios como función
-- La expresión final no considera el 0 y el 1 como neutros de de la adicción y multiplicación.
normArit :: AExp -> AExp
normArit arit = foldr f (Lit 0) wvars where -- 5
  f = \x y -> x:+:y
  vars = freeVars arit                      -- 1 
  weights = map (weightVar arit) vars       -- 2
  g (k, "") = (Lit k)                       -- 3
  g (k, x)  = k :*: (Var x)                 -- 3
  wvars = map g (zip weights vars)          -- 4


-- | SimplifyArit toma un AExp arit y retorna una versión que simplifica sobre el 0 y el 1.    
simplifyArit::AExp -> AExp
simplifyArit ((Lit 0) :+: arit) =  simplifyArit arit
simplifyArit (arit :+: (Lit 0)) =  simplifyArit arit
simplifyArit (Lit m) :+: (Lit n) = (Lit $ m + n) 
simplifyArit (arit_1 :+: arit_2) = (simplifyArit arit_1) :+: (simplifyArit arit_2)
simplifyArit (1 :*: arit) = simplifyArit arit
simplifyArit (0 :*: _) = (Lit 0)
simplifyArit (k :*: arit) = k :*: (simplifyArit arit)
simplifyArit otherwise = otherwise 

-- | Retorna una versión normalizada de un AExp.
completeNormArit :: AExp -> AExp
completeNormArit = simplifyArit.normArit

---------------------------------- { EXPRESIONES BOOLEANAS} ------------------------------------------
-- NOTA: Se podría agregar el :<: como operador, considerando que es super común como condición
-- NOTA: Se podría generalizar la estructura para que no sólo reciba AExp. 
{-
data BExp a = True'                        -- Constante True
            | False'                       -- Constante False
            | a :<=: a                     -- mayor igual entre expresiones a
            | a :==: a                     -- igualdad expresiones a
            | BExp a :|: BExp a             -- Or lógico
            | BExp a :&: BExp a               -- And Lógico
            | Not BExp a deriving (Show, Eq) -- Negación expresión booleana
-}
-----------------------------------------------------------------------------------------------------
-- | Definición de expresiones Boolenas
data BExp = True'                        -- Constante True
          | False'                       -- Constante False
          | AExp :<=: AExp               -- mayor igual entre expresiones aritméticas
          | AExp :==: AExp               -- igualdad expresiones aritméticas
          | BExp :|: BExp                -- Or lógico
          | BExp :&: BExp                -- And Lógico
          | Not BExp deriving (Show, Eq) -- Negación expresión booleana

---------------------------------- { FUNCIONES EXPRESIONES BOOLEANAS } ------------------------------------------

-- | Función de sustitución toma una variable "x", un AExp aritFor y una expresión booleana AritIn
-- reemplaza todas las incidendias de "x" en la expresión aritIn por la expresión aritFor.
sustBExp :: Name -> AExp -> BExp -> BExp
sustBExp _  _ True' = True'
sustBExp _  _ False' = False'
sustBExp x aritFor (e_1 :<=: e_2) = (sustAExp x aritFor e_1) :<=: (sustAExp x aritFor e_2)
sustBExp x aritFor (e_1 :==: e_2) = (sustAExp x aritFor e_1) :==: (sustAExp x aritFor e_2)
sustBExp x aritFor (e_1 :|: e_2) = (sustBExp x aritFor e_1) :|: (sustBExp x aritFor e_2)
sustBExp x aritFor (e_1 :&: e_2) = (sustBExp x aritFor e_1) :&: (sustBExp x aritFor e_2)
sustBExp x aritFor (Not e) = (Not (sustBExp x aritFor e))

-- | Función que entrega las variables libres de una expresión aritmética
freeVarsBExp :: BExp -> Names
freeVarsBExp True' = []
freeVarsBExp False' = []
freeVarsBExp (arit_1 :<=: arit_2) = (freeVars arit_1) ++ (freeVars arit_2)
freeVarsBExp (arit_1 :==: arit_2) = (freeVars arit_1) ++ (freeVars arit_2)
freeVarsBExp (b_1 :|: b_2) = (freeVarsBExp b_1) ++ (freeVarsBExp b_2)
freeVarsBExp (b_1 :&: b_2) = (freeVarsBExp b_1) ++ (freeVarsBExp b_2)
freeVarsBExp (Not b) = (freeVarsBExp b)


----------------------------------{ RUNTIMES }-----------------------------------------------------
-- ACOTACIÓN: Se podría parametrizar la estructura para que sea más general.
{-
data RunTime a = RunTimeArit a                        -- RunTime hecho a partir de una expresión a
             | BExp a :<>: RunTime a                  -- multiplicación por una condición
             | RunTime a:++: RunTime a                -- suma de RunTime
             | Constant :**: RunTime a deriving (Eq)  -- ponderación por constante 
-}
-- Creo que varias de las funciones que defino abajo sobre RUNTIMES se podrían simplificar si se extendiera
-- a functor, functor aplicativo o mónada (sobre todo las de simplificar)
-- Veo cierto parecido al constructor (:) de las listas a la suma de Runtime :++:
-- NOTA:       Sobre el conjunto de restricciones que se genera con los Runtime como base.
--            Si lo veo de un punto de vista geométrico, las restricciones son hiperplanos y su intersección son poliedros
--            Esto me recordó mucho los distintos problemas de optimización Real o mixta, dónde se 
--            relajan ciertas restricciones para llegar a una aproximación.
--            Quizás algunas se esas aproximaciones/relajaciones se podrían aplicar con el fin de tener más constructores
--            Aunque también es cierto que sería un gran trabajo extra, pero lo menciono para dejarlo como trabajo a futuro
-- | Definición de RunTimes
data RunTime = RunTimeArit AExp                     -- RunTime hecho a partir de una expresión aritmética
             | BExp :<>: RunTime                    -- multiplicación por una condición
             | RunTime :++: RunTime                 -- suma de RunTime
             | Constant :**: RunTime  deriving (Eq) -- ponderación por constante 

----------------------------------{ FUNCIONES RUNTIMES }-----------------------------------------------------

-- | Definición de método show para la clase
instance Show RunTime where 
  show (RunTimeArit arit) = show arit
  show (e_b :<>: (RunTimeArit (Lit 1))) = "["++ (show e_b) ++ "]"
  show (e_b :<>: (RunTimeArit (Lit n))) = "["++ (show e_b) ++ "]*" ++ (show n)
  show (e_b :<>: runt) = "["++ (show e_b) ++ "]*" ++ "(" ++ (show runt) ++ ")"
  show (e_1 :++: e_2) =  show e_1 ++" + "++ show e_2    
  show (e_1 :**: e_2) = show e_1 ++" * " ++ show e_2        

-- | Función de sustitución toma una variable "x", un AExp aritFor, un RunTime runtIn
-- reemplaza todas las indicendias de "x" en la expresión runtIn por la expresión aritFor.
sustRunTime :: Name -> AExp -> RunTime -> RunTime
sustRunTime x aritFor (RunTimeArit aritIn) = (RunTimeArit (sustAExp x aritFor aritIn))
sustRunTime x aritFor (e_b :<>: e_r) = (sustBExp x aritFor e_b) :<>: (sustRunTime x aritFor e_r)
sustRunTime x aritFor (e_1 :++: e_2) = (sustRunTime x aritFor e_1) :++: (sustRunTime x aritFor e_2)
sustRunTime x aritFor (k :**: e) = k :**: (sustRunTime x aritFor e)

-- | Entrega las variables libres dentro de un Runtime
freeVarsRunTime :: RunTime -> Names
freeVarsRunTime (RunTimeArit arit) = freeVars arit
freeVarsRunTime (b :<>: runt) = (freeVarsBExp b) ++ (freeVarsRunTime runt)
freeVarsRunTime (e_1 :++: e_2) = (freeVarsRunTime e_1) ++ (freeVarsRunTime e_2)

----------------------------------{ PROGRAMAS }-----------------------------------------------------
-- NOTA : Se podría agregar el ciclo for
-- La versión general 
-- data Program = For (Set Name AExp) (Set Name AExp) BExp Program
-- Las partes son : iniciar variable - modificación al final de cada ciclo - condición de fin - cuerpo del for
-- Sería basicamente un while, así que no creo que aporte mucho en la práctica.
-- Versión simplificada
-- data Program = For Integer Program
-- Representa iterar una versión constante de veces el cuerpo del for.
-- La idea es que sea sólo azúcar sintáctica y luego yo por debajo lo transforme a su equivalente
-- del tipo Seq Program Program para luego usar la transformada sobre él.

data Program = Skip -- programa vacío que toma una unidad de tiempo
            | Empty -- programacio vacío sin costo de tiempo
            | Set Name AExp -- Asignación
            | Seq Program Program -- Composición secuencial de programas
            | If BExp Program Program -- guarda condicional
            | While BExp Program RunTime deriving(Show, Eq) -- ciclo while

----------------------------------{ RESTRICCIONES }-----------------------------------------------------

-- | Definición de restriccion
data Restriction a = a :!==: a 
                   | a :!<=: a deriving (Eq, Show)

-- | Extender a Functor
instance Functor Restriction  where
-- fmap :: (a -> b) -> Restriction a -> Restriction b  
  fmap f (a_1 :!==: a_2) = ((f a_1 ):!==: (f a_2))
  fmap f (a_1 :!<=: a_2) = ((f a_1 ):!<=: (f a_2))

-- | Extender a Functor Aplicativo
instance Applicative Restriction where
-- pure :: a -> Restriction a
  pure a = a :!==: a
-- (<*>) :: Restriction (a->b) -> Restriction a -> Restriction b
  (f_1 :!==: f_2 ) <*> (a_1 :!<=: a_2) = ((f_1 a_1 ):!<=: (f_2 a_2))
  (f_1 :!==: f_2 ) <*> (a_1 :!==: a_2) = ((f_1 a_1 ):!==: (f_2 a_2))
  (f_1 :!<=: f_2 ) <*> (a_1 :!<=: a_2) = ((f_1 a_1 ):!<=: (f_2 a_2))
  (f_1 :!<=: f_2 ) <*> (a_1 :!==: a_2) = ((f_1 a_1 ):!==: (f_2 a_2))
  
-- | Extender a Mónada 
-- NOTA: Creo que esta está demás, ya que no veo una formal natural de usar la def de mónada.
instance Monad Restriction where
-- (>>=) :: Restriction a -> (a -> Restriction b) -> Restriction b
  (a :!==: _) >>= f = f a
  (a :!<=: _) >>= f = f a

-- | Definición de un función de fold para la estructura Restriction
foldRes :: (b -> b -> c) -> (a -> b) -> Restriction a -> c
foldRes f g (e_1 :!==: e_2) = f (g e_1) (g e_2)
foldRes f g (e_1 :!<=: e_2) = f (g e_1) (g e_2)

---------------------------- { SINÓNIMOS DE TIPOS ÚTILES } ---------------------------------------------

type RArit =  Restriction AExp
type RRunTime =  Restriction RunTime 


----------------------------------{ VC GEN }-------------------------------------------------------------
-- | Generador de restricciones 
-- entrega un conjunto de restricciones y el tiempo de ejecución esperado
vcGenerator ::  Program -> RunTime -> (RunTime, [RRunTime])
vcGenerator Skip runt = ((RunTimeArit (Lit 1)) :++: runt, [])
vcGenerator Empty runt = (runt, [])
vcGenerator (Set x arit ) runt = ((RunTimeArit (Lit 1)) :++: (sustRunTime x arit runt), [])
vcGenerator (If e_b e_t e_f) runt = ( (RunTimeArit (Lit 1)) :++:((e_b :<>:fst vc_t):++:((Not e_b):<>: fst vc_f)),  (snd vc_t) ++ (snd vc_f)) where 
    vc_t = vcGenerator e_t runt
    vc_f = vcGenerator e_f runt
vcGenerator (Seq p_1 p_2) runt = (fst vc_1, (snd vc_1) ++ (snd vc_2))   where
    vc_2 = vcGenerator p_2 runt
    vc_1 = vcGenerator p_1 (fst vc_2)
vcGenerator (While e_b p inv) runt = (inv, [l_inv :!<=: inv] ++ (snd vc_p)) where
    vc_p = vcGenerator p inv
    l_inv = (RunTimeArit (Lit 1) :++: ( ((Not e_b) :<>: runt):++: (e_b :<>: (fst vc_p))))

---------------------------{ SIMPLIFICAR EXPRESIONES BOOLEANAS }---------------------------------------------------------
-- | Reglas de un sólo paso para simplificar un BExp
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

-- | Reglas recursivas para simplificar un BExp
deepSimplifyBExp :: BExp -> BExp
deepSimplifyBExp True' = True'
deepSimplifyBExp False' = False'
deepSimplifyBExp (e_1 :<=: e_2) = (completeNormArit e_1) :<=: (completeNormArit e_2)
deepSimplifyBExp (e_1 :==: e_2) = (completeNormArit e_1) :==: (completeNormArit e_2)
deepSimplifyBExp (e_1 :|: e_2) = simplifyBExp ((deepSimplifyBExp e_1) :|: (deepSimplifyBExp e_2))
deepSimplifyBExp (e_1 :&: e_2) = simplifyBExp ((deepSimplifyBExp e_1) :&: (deepSimplifyBExp e_2))
deepSimplifyBExp (Not e_b) = simplifyBExp (Not (deepSimplifyBExp e_b))

-------------------- {  SIMPLIFICAR RUNTIMES }------------------------------------------------------------------
-- Acá es donde creo que si tomase un enfoque monádico, se podría simplificar la expresión.
-- | Reglas de un sólo paso para simplificar un RunTime
simplifyRunTime :: RunTime -> RunTime
simplifyRunTime ((RunTimeArit (Lit m)) :++: (RunTimeArit (Lit n))) = (RunTimeArit (Lit (n + m)))
simplifyRunTime ((RunTimeArit (Lit m)) :++: ((RunTimeArit (Lit n)) :++: runt)) = (RunTimeArit (Lit (n + m)) :++: runt)
simplifyRunTime ( True' :<>: runt) = runt
simplifyRunTime (False' :<>: _ ) = (RunTimeArit (Lit 0))
simplifyRunTime ((RunTimeArit (Lit 0)) :++: runt) = runt
simplifyRunTime (runt :++: (RunTimeArit (Lit 0))) = runt
simplifyRunTime (1 :**: runt) = runt
simplifyRunTime (0 :**: _ ) = (RunTimeArit (Lit 0))
simplifyRunTime otherwise = otherwise

-- Reglas recursivas para simplificar un RunTime
deepSimplifyRunTime :: RunTime -> RunTime
deepSimplifyRunTime (RunTimeArit arit) = (RunTimeArit (completeNormArit arit))
deepSimplifyRunTime (bexp :<>: runt) = simplifyRunTime ((deepSimplifyBExp bexp) :<>: (deepSimplifyRunTime runt))
deepSimplifyRunTime (e_1 :++: e_2) = simplifyRunTime ((deepSimplifyRunTime e_1) :++: (deepSimplifyRunTime e_2))
deepSimplifyRunTime (k :**: runt) = simplifyRunTime (k :**: (deepSimplifyRunTime runt))



---------------------------{ PREPARACIÓN PARA SBV }-------------------------------------------------

type Context = [BExp]
type Contexts = [Context]
type SolverInput = (Context,RArit, Names)

-- | Retorna todas las instancias de BExp dentro un RunTime sin repeticiones
findConditionRunTime :: RunTime -> Context
findConditionRunTime runt = rmdups conds where
  conds = f runt 
  f (RunTimeArit _) = []
  f ((Not bexp) :<>: runt) = [bexp] ++ (findConditionRunTime runt)
  f (bexp :<>: runt) = [bexp] ++ (findConditionRunTime runt)
  f (e_1 :++: e_2) = (findConditionRunTime e_1) ++ (findConditionRunTime e_2)
  f (_ :**: runt) = (findConditionRunTime runt)

-- | Toma un RunTime runt y retorna todos los posibles context (matriz de BExp) que 
-- se pueden extraer a partir de los BExp que tiene el RunTime.
allContext :: RunTime -> Contexts
allContext runt = map (zipWith f conds) lbools where
  f bexp True = bexp
  f bexp _ = (Not bexp)
  conds = findConditionRunTime runt
  lbools = bools (length conds)

-- | Toma un BExp bexp y un RunTime runt, evalua todas las instancias de bexp 
-- dentro de runt
evalCondition :: BExp -> RunTime -> RunTime
evalCondition bexp (RunTimeArit arit) = (RunTimeArit arit)
evalCondition bexp1 (bexp2 :<>: runt) | bexp1 == bexp2 = (evalCondition bexp1 runt)
                                      | (deepSimplifyBExp (Not bexp1)) == bexp2 = (RunTimeArit (Lit 0))
                                      | otherwise = (bexp2 :<>: (evalCondition bexp1 runt))
evalCondition bexp (e_1 :++: e_2) = (evalCondition bexp e_1) :++: (evalCondition bexp e_2)
evalCondition bexp (k :**: runt) = k :**: (evalCondition bexp runt)

-- | Toma un RunTime runt y retorna su versión AExp en el caso de que se pueda
-- NOTA: En un principio pensé en usar la mónada maybe para que el porgrama no se caiga
-- pero en los ejemplos que vi siempre tiraban el error, así que quise seguir esa línea
runTimeToArit :: RunTime -> AExp
runTimeToArit (RunTimeArit arit) = arit
runTimeToArit (e_1 :++: e_2) = (runTimeToArit e_1) :+: (runTimeToArit e_2)
runTimeToArit (k :**: e) = k :*: (runTimeToArit e)
runTimeToArit otherwise = error $ "No hay versión directa a AExp" ++ otherwise

-- Versión monádica de la función anterior
runTimeToArit' :: RunTime -> Maybe AExp
runTimeToArit' (RunTimeArit arit) = Just arit
runTimeToArit' (e_1 :++: e_2) = do 
                            aexp1 <- runTimeToArit' $ e_1
                            aexp2 <- runTimeToArit' $ e_2
                            return (aexp1 :+: aexp2)
runTimeToArit' (k :**: e) = do 
                            aexp <- runTimeToArit' $ e
                            return ( k :*: aexp) 
runTimeToArit' otherwise = Nothing

-- | Toma una restricción de Runtime a:!<=:b y retorna un arreglo de restricciones de AExp [a:<=:b]
-- Uno por cada contexto posible
---------------------------------------------------------------------------------------------------
-- NOTA: Este algoritmo es poco claro y creo que debe cambiarse.
-- Descripción del algoritmo
-- 0 Entrega una un arreglo de 3-tuplas (SolverInpur)
-- Cada tupla es
-- 0.a. Context: Un arreglo de BExp, es la hiṕotesis del implica 
-- 0.a. Ejemplo [x<y, y<0]
-- 0.b. Restricción: Restriction RArit, será la conclusión del Implica
-- 0.b. Ejemplo x<8
-- 0.c. Variables libres de todo el SolverInput 
-- 0.c Ejemplo [x, y]

-- 1. Simplificar los dos runtimes de la restricción a:!<=:b -> a':!<=:b'
-- 2. Extraer todos los contextos posibles de los dos runtime a' y b'
-- 3. Función currificada, para poder evaluar una condición y usarlar con los contexts
-- 4. Evaluar todos los contextos y generar todas las posibles restricciones de runtimes [a'':!<=:b'' ]
-- 5. A partir de las restricciones de runtimes [a'':<=:b''], se extrae la expresión aritmetica subyacente 
--    generando restricciones de AExp [a''' :!<=b'''].
-- 6. Funcion que toma un context [BExp] y extrae las variables libres y las concatena
-- 7. Arreglo cuyos elementos son arreglos con todas las varibles libres de cada posible contexto
-- 8. Variables libres de cada restricción de AExp [a''' :!<=b''']
-- 9. Uno las variables libre de cada context con las variables de su respectiva restricción
-- 10. Elimino las variables libres que sean "", ya que en realidad son Literales (Lit n)
restrictionsToSolver :: RRunTime -> [SolverInput]
restrictionsToSolver rest = zip3 contexts eval_arit free_vars' where  -- 0
  simplify_rest = (fmap deepSimplifyRunTime rest)                     -- 1 
  contexts = (allContext (foldRes (:++:) (id) simplify_rest))         -- 2
  f  = \bexp -> fmap (evalCondition bexp)                             -- 3
  eval_runt = map (foldr f simplify_rest) contexts                    -- 4
  eval_arit = map (fmap $ completeNormArit.runTimeToArit) eval_runt   -- 5
  g = \ context -> foldr (++) [] (map freeVarsBExp context)           -- 6
  free_vars_bool = map g contexts                                     -- 7
  free_vars_rest = map (foldRes (++) freeVars) eval_arit              -- 8
  free_vars = map rmdups (zipWith (++) free_vars_bool free_vars_rest) -- 9
  free_vars' = map (filter  (/="")) free_vars                         -- 10


---------------------------- {FUNCIONES PARA IMPRIMIR EN PANTALLA TODO EL INPUT }---------------------------------------------------------
showSolverInput :: SolverInput -> Int -> IO()
showSolverInput (contexto, rest, vars) n = do 
  putStr "Par (contexto, restricción) número " 
  print n
  putStrLn "El contexto es :"
  print contexto
  putStrLn "Las variables libres son :"
  print vars
  putStrLn "La restricción es :"
  print rest
  print $ concat (replicate 50 "-")

showSolverInputs ::RRunTime -> IO()
showSolverInputs runtr = do
  print $ concat (replicate 50 "*")
  putStrLn "La restricción es :"
  print runtr 
  putStr "Hay un total de " 
  putStr.show $ n 
  putStrLn " diferentes pares (contexto, restricción) " 
  print $ concat (replicate 50 "-")
  mapM_ (uncurry showSolverInput ) $ zip  inputs [1..n]
  print $ concat (replicate 50 "*")
  where 
    n = length (restrictionsToSolver runtr) 
    inputs = restrictionsToSolver runtr



---------------------------(Programas de ejemplo)---------------------------------------------------------

-- programa de ejemplo 

p0 = (While False' Empty (RunTimeArit (Lit 5)))
vc_0 = vcGenerator p0 (RunTimeArit (Lit 0)) 
r0 = fst vc_0
s0 = deepSimplifyRunTime (fst vc_0)

-- programa 1 de ejemplo

l0 = Set  "x" ((Var "x"):+:(Lit 1)) --cambiar por menos
l3 = Set "y" (2:*:(Var "x"))
l6 = Set "w" (Lit 3)
l7 = Set "x" ((Var  "w") :+: (Var "x"))
l9 = Set "y" (Lit 5)
l5_9 = If ((Lit 8) :<=: (Var "w")) (Seq l6 l7) l9
l1_9 = If ((Var "y") :<=: (Var "x")) (Seq Skip l3) l5_9
l0_9 = Seq l0 l1_9

vc_1 = vcGenerator l0_9 (RunTimeArit (Lit 0))
r1 = fst vc_1
s1 = deepSimplifyRunTime (fst vc_1)


---------------------------(Restricciones de ejemplo)---------------------------------------------------------
c1 = (2 :*:(Var "x")):<=:(Var "y")
c2 = (Var "x"):<=:(Lit 0)
c3 = (Var "x"):<=:(Lit 0)
c4 = (Var "ww"):<=:(Lit 0)

arit1 = (Var "x") :+: (Var "y")
rarit1 = RunTimeArit arit1

arit2 = (Var "y") :+: (Lit 1)
rarit2 = 10 :**: (RunTimeArit arit2)
rarit3 = rarit1 :++: rarit2


runt1 = c1 :<>: (RunTimeArit arit1)
runt2 = c2 :<>: (RunTimeArit arit2)
restriction = runt1 :!<=: runt2

z3_rest = restrictionsToSolver restriction
------------------------------------------------------------------------------------------------------------------------------------
{-
sumando1 = (Not( (Lit 8 ):<=: Var "w")):<>: (RunTimeArit(Lit 4)) 
sumando2 = ( (Lit 8):<=: Var "w"):<>: (RunTimeArit(Lit 5))

runtr = sumando1 :++: sumando2

res = s1 :!<=: runtr

ejemploP3 = restrictionsToSolver res

ejSol = ejemploP3 !! 0
printEj = showSolverInputs res
-}
-------------------------------(Simplificaciónes Aritméticas)--------------------------------------------------------

a1 = (((((Var "x") :+: (Var "x")) :+: (Lit 1)) :+: ((-2) :*: (Var "y"))) :+: (Lit 8))

sa1 = completeNormArit $ a1
-------------------------------(Simplificaciónes Aritméticas)--------------------------------------------------------
