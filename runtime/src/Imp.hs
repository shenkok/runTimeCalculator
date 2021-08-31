module Imp where

import Data.List
import Data.Ratio

{-
  MODULO QUE SE ENCARGA SE REPRESENTAR EXPRESIONES ARITMÉTICAS, BOOLEANAS, RUNTIMES Y PROGRAMAS
-}
-- <@Fede: Name se utilizará para el nombre de qué cosas?; agregar comentario>
-- @Luis: Hecho
type Name = String -- Nombre de las variables

type Names = [Name]

type Constant = Rational --  Constantes Númericas

-- TODO: Parser para escribir los lenguajes de forma cómoda
-- TODO: Averigüar como bajar el número de parentesis

---------------------------------------- { FUNCIONES ÚTILES }---------------------------------------

-- | Elimina los elementos repetidos en una lista
rmdups :: (Eq a) => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- | bools retorna una matriz con todas las posibles combinaciones False/True
-- de tamaño n.
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) r ++ map (True :) r
  where
    r = bools (n -1)

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

-- <@Fede: sería bueno mencionar esta linea en la parte de trabajo futuro>

data AExp
  = Lit Constant -- Números
  | Var Name -- Variables x, y, z
  | AExp :+: AExp -- Suma de expresiones aritméticas
  | Constant :*: AExp
  deriving (Eq) -- Ponderación por una constante

---------------------------------------- { FUNCIONES EXPRESIONES ARITMÉTICAS }--------------------------------

-- | Definición del método show para AExp
instance Show AExp where
  show (Lit n)        = show n
  show (Var x)        = show x
  show (e_1 :+: e_2)  = show e_1 ++ " + " ++ show e_2
  show (k :*: Lit n)  = show k   ++ "*" ++ show n
  show (k :*: Var x)  = show k   ++ "*" ++ show x 
  show (k :*: e_2)    = show k   ++ "*(" ++ show e_2 ++")"

-- | Sustituye todas las instancias "x" en AritIn y por aritFor
sustAExp :: Name -> AExp -> AExp -> AExp
sustAExp _ _ (Lit n)             = Lit n
sustAExp x aritFor (Var y)       = if x == y then aritFor else Var y
sustAExp x aritFor (e_1 :+: e_2) = sustAExp x aritFor e_1 :+: sustAExp x aritFor e_2
sustAExp x aritFor (k :*: e)     = k :*: sustAExp x aritFor e

-- | Toma un AExp arit y retorna una lista de todas las variables libres
-- considerando que un número está asociado a la variable vacía "".
freeVars :: AExp -> Names
freeVars arit = sort (rmdups (fvar arit))
  where
    fvar (Lit _)       = [""]
    fvar (Var x)       = [x]
    fvar (e_1 :+: e_2) = fvar e_1 ++ fvar e_2
    fvar (_ :*: e)     = fvar e

-- | WeightVar toma un Aexp arit y una variable var, retorna el peso suma de todas las instancias de esa variable var
-- en el AExp arit, se considera n:*:"" como equivalente a (Lit n) .
weightVar :: AExp -> Name -> Constant
weightVar (Lit n) var
  | var == "" = n
  | otherwise = 0
weightVar (Var x) var
  | var == x = 1
  | otherwise = 0
weightVar (e_1 :+: e_2) var = weightVar e_1 var + weightVar e_2 var
weightVar (k :*: e) var     = k * weightVar e var

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
normArit arit = foldr (:+:) (Lit 0) wvars -- 5
  where
    vars = freeVars arit -- 1
    weights = map (weightVar arit) vars -- 2
    g (k, "") = Lit k -- 3
    g (k, x) = k :*: Var x -- 3
    wvars = zipWith (curry g) weights vars -- 4

-- | SimplifyArit toma un AExp arit y retorna una versión que simplifica sobre el 0 y el 1. @Fede: y suma literales
simplifyArit :: AExp -> AExp
simplifyArit (Lit 0 :+: arit)  = simplifyArit arit
simplifyArit (arit  :+: Lit 0)  = simplifyArit arit
simplifyArit (arit_1 :+: arit_2) = simplifyArit arit_1 :+: simplifyArit arit_2
simplifyArit (1 :*: arit)        = simplifyArit arit
simplifyArit (0 :*: _)           = Lit 0
simplifyArit (k :*: arit)        = k :*: simplifyArit arit
simplifyArit otherwise           = otherwise

-- | Retorna una versión normalizada de un AExp.
completeNormArit :: AExp -> AExp
completeNormArit = simplifyArit . normArit

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
data BExp
  = True' -- Constante True
  | False' -- Constante False
  | AExp :<=: AExp -- mayor igual entre expresiones aritméticas
  | AExp :==: AExp -- igualdad expresiones aritméticas
  | BExp :|: BExp -- Or lógico
  | BExp :&: BExp -- And Lógico
  | Not BExp
  deriving (Show, Eq) -- Negación expresión booleana

---------------------------------- { AZÚCAR SINTÁCTICA BOOLEANAS} ----------------------------------------------

-- | Azúcar sintáctica para >=
(>=:) :: AExp -> AExp -> BExp
(>=:) arit_1 arit_2 = arit_2 :<=: arit_1

-- | Azúcar sintáctica para >
(>:) :: AExp -> AExp -> BExp
(>:) arit_1 arit_2 = Not $ arit_1 :<=: arit_2

-- | Azúcar sintáctica para <
(<:) :: AExp -> AExp -> BExp
(<:) arit_1 arit_2 = Not $ arit_2 :<=: arit_1

-- | Azúcar sintáctica para /=
(/=:) :: AExp -> AExp -> BExp
(/=:) arit_1 arit_2 = Not $ arit_1 :==: arit_2

---------------------------------- { FUNCIONES EXPRESIONES BOOLEANAS } ------------------------------------------

-- | Función de sustitución toma una variable "x", un AExp aritFor y una expresión booleana AritIn
-- reemplaza todas las incidendias de "x" en la expresión aritIn por la expresión aritFor.
sustBExp :: Name -> AExp -> BExp -> BExp
sustBExp _ _ True'                = True'
sustBExp _ _ False'               = False'
sustBExp x aritFor (e_1 :<=: e_2) = sustAExp x aritFor e_1 :<=: sustAExp x aritFor e_2
sustBExp x aritFor (e_1 :==: e_2) = sustAExp x aritFor e_1 :==: sustAExp x aritFor e_2
sustBExp x aritFor (e_1 :|: e_2)  = sustBExp x aritFor e_1 :|: sustBExp x aritFor e_2
sustBExp x aritFor (e_1 :&: e_2)  = sustBExp x aritFor e_1 :&: sustBExp x aritFor e_2
sustBExp x aritFor (Not e)        = Not (sustBExp x aritFor e)

-- | Función que entrega las variables libres de una expresión aritmética
freeVarsBExp :: BExp -> Names
freeVarsBExp True'                = []
freeVarsBExp False'               = []
freeVarsBExp (arit_1 :<=: arit_2) = freeVars arit_1 ++ freeVars arit_2
freeVarsBExp (arit_1 :==: arit_2) = freeVars arit_1 ++ freeVars arit_2
freeVarsBExp (b_1 :|: b_2)        = freeVarsBExp b_1 ++ freeVarsBExp b_2
freeVarsBExp (b_1 :&: b_2)        = freeVarsBExp b_1 ++ freeVarsBExp b_2
freeVarsBExp (Not b)              = freeVarsBExp b

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
-- NOTA:       Sobre el conjunto de restricciones que se genera con los Runtime como base.
--            Si lo veo de un punto de vista geométrico, las restricciones son hiperplanos y su intersección son poliedros
--            Esto me recordó mucho los distintos problemas de optimización Real o mixta, dónde se
--            relajan ciertas restricciones para llegar a una aproximación.
--            Quizás algunas se esas aproximaciones/relajaciones se podrían aplicar con el fin de tener más constructores
--            Aunque también es cierto que sería un gran trabajo extra, pero lo menciono para dejarlo como trabajo a futuro

-- | Definición de RunTimes
data RunTime
  = RunTimeArit AExp -- RunTime hecho a partir de una expresión aritmética
  | BExp :<>: RunTime -- multiplicación por una condición
  | RunTime :++: RunTime -- suma de RunTime
  | Constant :**: RunTime
  deriving (Eq) -- ponderación por constante

----------------------------------{ FUNCIONES RUNTIMES }-----------------------------------------------------

----------------------------------{ AZÚCAR SINTÁCTICA } -----------------------------------------------------
-- | Azúcar sintáctica para el 0 runtime 
rtZero :: RunTime
rtZero = RunTimeArit (Lit 0)
-- | Azúcar sintáctica para el 1 runtime
rtOne :: RunTime
rtOne = RunTimeArit (Lit 1)

-- | Azúcar sintáctica para un literal runtime 
rtLit :: Constant -> RunTime
rtLit k = RunTimeArit (Lit k)

-- | Azúcar sintáctica para un var runtime
rtVar :: Name -> RunTime
rtVar x = RunTimeArit (Var x) 

-- | Definición de método show para la clase
{-
instance Show RunTime where
  show (RunTimeArit arit)               = show arit
  show (e_b :<>: rtOne)                 = "[" ++ show e_b ++ "]"
  show (e_b :<>: rtLit n)               = "[" ++ show e_b ++ "]*" ++ show n
  show (e_b :<>: rtVar x)               = "[" ++ show e_b ++ "]*" ++ show x
  show (e_b :<>: runt)                  = "[" ++ show e_b ++ "]*" ++ "(" ++ show runt ++ ")"
  show (e_1 :++: e_2)                   = show e_1 ++ " + " ++ show e_2
  show (k :**: rtLit n)                 = show k ++ "*" ++ show n
  show (k :** rtVar x)                  = show k ++ "*" ++ show k
  show (k :**: e_2)                     = show k ++ " * (" ++ show e_2 ++ ")"
  -}
                                                                     -- @Fede: cambiaría e_1 por k, para dar cuenta de que representan expresiones de distinto tipo
instance Show RunTime where                                           -- @Luis: Hecho
  show (RunTimeArit arit)               = show arit
  show (e_b :<>: RunTimeArit (Lit 1))   = "[" ++ show e_b ++ "]"
  show (e_b :<>: RunTimeArit (Lit n))   = "[" ++ show e_b ++ "]*" ++ show n
  show (e_b :<>: RunTimeArit (Var x))   = "[" ++ show e_b ++ "]*" ++ show x
  show (e_b :<>: runt)                  = "[" ++ show e_b ++ "]*" ++ "(" ++ show runt ++ ")"
  show (e_1 :++: e_2)                   = show e_1 ++ " + " ++ show e_2
  show (k :**: RunTimeArit (Lit n))     = show k ++ "*" ++ show n
  show (k :**: RunTimeArit (Var x))     = show k ++ "*" ++ show k
  show (k :**: e_2)                     = show k ++ " * (" ++ show e_2 ++ ")"
-- | Función de sustitución toma una variable "x", un AExp aritFor, un RunTime runtIn
-- reemplaza todas las indicendias de "x" en la expresión runtIn por la expresión aritFor.
sustRunTime :: Name -> AExp -> RunTime -> RunTime
sustRunTime x aritFor (RunTimeArit aritIn) = RunTimeArit (sustAExp x aritFor aritIn)
sustRunTime x aritFor (e_b :<>: e_r)       = sustBExp x aritFor e_b :<>: sustRunTime x aritFor e_r
sustRunTime x aritFor (e_1 :++: e_2)       = sustRunTime x aritFor e_1 :++: sustRunTime x aritFor e_2
sustRunTime x aritFor (k :**: e)           = k :**: sustRunTime x aritFor e

-- | Entrega las variables libres dentro de un Runtime
freeVarsRunTime :: RunTime -> Names
freeVarsRunTime (RunTimeArit arit) = freeVars arit
freeVarsRunTime (b :<>: runt)      = freeVarsBExp b ++ freeVarsRunTime runt
freeVarsRunTime (e_1 :++: e_2)     = freeVarsRunTime e_1 ++ freeVarsRunTime e_2
freeVarsRunTime (_ :**: e)         = freeVarsRunTime e

----------------------------------{ CONSTRUCCIONES PROBABILISTAS} ----------------------------------
type PConstant        = Constant
type Distribution a   = [(PConstant, a)]
type PAExp = Distribution AExp
type PBExp = Distribution Bool

massDistribution :: Distribution a -> PConstant
massDistribution p_x = foldr (+) 0 (fst.unzip $ p_x)

isDistribution :: Distribution a -> Bool
isDistribution p_x = massDistribution p_x == 1

bernoulli :: PConstant -> PBExp
bernoulli p = [(p, True), (1-p, False)]

-- | Nota: se podría hacer de manera monádica
bernoullip :: PBExp -> Bool -> PConstant
bernoullip ((p, True):xs) True  = p 
bernoullip ((p, True):xs)  False = 1-p
bernoullip ((p, False):xs) True  = p 
bernoullip ((p, False):xs) False = 1-p

uniformN :: Integer -> Distribution AExp
uniformN n = zip (repeat $ 1%n) (map Lit [1..(n%1)])

-- | Calculo de esperanza. toma una distribución, una función de transformacion para un a
--, una función * que retorna un c, una función + que retorna un d, un caso base para el fold y retorna un tipo d
expectation :: Distribution a -> (a -> b) -> (PConstant -> b -> c)-> (c -> d -> d) -> d -> d
expectation p_x h prod sum base =  foldr sum base (map f p_x) where
  f (k, e) = prod k (h  e)  

-- | esperanza para dsitribuciones obre expresiones aritméticas
aexpE :: Distribution AExp -> Name -> RunTime -> RunTime 
aexpE p_x x runt = deepSimplifyRunTime $ expectation p_x f (:**:) (:++:) rtZero where
  f arit = sustRunTime x arit runt  


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

data Program
  = Skip -- programa vacío que toma una unidad de tiempo
  | Empty -- programacio vacío sin costo de tiempo
  | Set Name AExp -- Asignación
  | PSet Name PAExp -- Asignación probabilista
  | Seq Program Program -- Composición secuencial de programas
  | If BExp Program Program -- guarda condicional
  | PIf PBExp Program Program -- guarda condicional probabilista
  | While BExp Program RunTime
  | PWhile PBExp Program RunTime
  deriving (Show, Eq) -- ciclo while

----------------------------------{ RESTRICCIONES }-----------------------------------------------------

-- | Definición de restriccion
data Restriction a
  = a :!==: a
  | a :!<=: a
  deriving (Eq, Show)

-- | Extender a Functor
instance Functor Restriction where
  -- fmap :: (a -> b) -> Restriction a -> Restriction b
  fmap f (a_1 :!==: a_2) = f a_1 :!==: f a_2
  fmap f (a_1 :!<=: a_2) = f a_1 :!<=: f a_2

-- | Extender a Functor Aplicativo
instance Applicative Restriction where
  -- pure :: a -> Restriction a
  pure a = a :!==: a

  -- (<*>) :: Restriction (a->b) -> Restriction a -> Restriction b
  (f_1 :!==: f_2) <*> (a_1 :!<=: a_2) = f_1 a_1 :!<=: f_2 a_2
  (f_1 :!==: f_2) <*> (a_1 :!==: a_2) = f_1 a_1 :!==: f_2 a_2
  (f_1 :!<=: f_2) <*> (a_1 :!<=: a_2) = f_1 a_1 :!<=: f_2 a_2
  (f_1 :!<=: f_2) <*> (a_1 :!==: a_2) = f_1 a_1 :!==: f_2 a_2

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

type RArit = Restriction AExp

type RRunTime = Restriction RunTime

----------------------------------{ VC GEN }-------------------------------------------------------------

-- | Generador de restricciones y calcula un candidato a cota superior
-- entrega un conjunto de restricciones y el tiempo de ejecución esperado
vcGenerator :: Program -> RunTime -> (RunTime, [RRunTime])
vcGenerator Skip runt                = (rtOne :++: runt, [])
vcGenerator Empty runt               = (runt, [])
vcGenerator (Set x arit) runt        = (rtOne :++: sustRunTime x arit runt, [])
vcGenerator (PSet x parit) runt      = (rtOne :++: aexpE parit x runt, [])
vcGenerator (If e_b e_t e_f) runt    = (rtOne :++: ((e_b :<>: fst vc_t) :++: (Not e_b :<>: fst vc_f)), snd vc_t ++ snd vc_f)
  where
    vc_t = vcGenerator e_t runt
    vc_f = vcGenerator e_f runt
vcGenerator (PIf pe_b e_t e_f) runt  = (rtOne :++: ((p_true :**: fst vc_t) :++: (p_false :**: fst vc_f)), snd vc_t ++ snd vc_f)
  where
    p_true = bernoullip pe_b True
    p_false = 1 - p_true
    vc_t = vcGenerator e_t runt
    vc_f = vcGenerator e_f runt
vcGenerator (Seq p_1 p_2) runt       = (fst vc_1, snd vc_1 ++ snd vc_2)
  where
    vc_2 = vcGenerator p_2 runt
    vc_1 = vcGenerator p_1 (fst vc_2)
vcGenerator (While e_b p inv) runt   = (inv, (l_inv :!<=: inv) : snd vc_p)
  where
    vc_p = vcGenerator p inv
    l_inv = rtOne :++: ((Not e_b :<>: runt) :++: (e_b :<>: fst vc_p))
vcGenerator (PWhile pe_b p inv) runt = (inv, (l_inv :!<=: inv) : snd vc_p)
  where
    p_true = bernoullip pe_b True
    p_false = 1 - p_true
    vc_p = vcGenerator p inv
    l_inv = rtOne :++: ((p_false :**: runt) :++: (p_true :**: fst vc_p))


-- | Genera las restricciones considerando al 0 como runtime
vcGenerator0 :: Program -> (RunTime, [RRunTime])
vcGenerator0 program = vcGenerator program rtZero

---------------------------{ SIMPLIFICAR EXPRESIONES BOOLEANAS }---------------------------------------------------------

-- | Reglas de un sólo paso para simplificar un BExp
simplifyBExp :: BExp -> BExp
simplifyBExp (True' :|: _)    = True'
simplifyBExp (_ :|: True')    = True'
simplifyBExp (e_b :|: False') = e_b
simplifyBExp (False' :|: e_b) = e_b
simplifyBExp (False' :&: _)   = False'
simplifyBExp (_ :&: False')   = False'
simplifyBExp (True' :&: e_b)  = e_b
simplifyBExp (e_b :&: True')  = e_b
simplifyBExp (Not (Not e_b))  = e_b
simplifyBExp otherwise        = otherwise

-- | Reglas recursivas para simplificar un BExp
deepSimplifyBExp :: BExp -> BExp
deepSimplifyBExp True'          = True'
deepSimplifyBExp False'         = False'
deepSimplifyBExp (e_1 :<=: e_2) = completeNormArit e_1 :<=: completeNormArit e_2
deepSimplifyBExp (e_1 :==: e_2) = completeNormArit e_1 :==: completeNormArit e_2
deepSimplifyBExp (e_1 :|: e_2)  = simplifyBExp (deepSimplifyBExp e_1 :|: deepSimplifyBExp e_2)
deepSimplifyBExp (e_1 :&: e_2)  = simplifyBExp (deepSimplifyBExp e_1 :&: deepSimplifyBExp e_2)
deepSimplifyBExp (Not e_b)      = simplifyBExp (Not $ deepSimplifyBExp e_b)

-------------------- {  SIMPLIFICAR RUNTIMES }------------------------------------------------------------------
-- Acá es donde creo que si tomase un enfoque monádico, se podría simplificar la expresión.

-- | Reglas de un sólo paso para simplificar un RunTime
simplifyRunTime :: RunTime -> RunTime
simplifyRunTime (RunTimeArit (Lit m) :++: RunTimeArit (Lit n))             = rtLit (m + n)
simplifyRunTime (RunTimeArit (Lit m) :++: (RunTimeArit (Lit n) :++: runt)) = (rtLit $ m + n) :++: runt
simplifyRunTime (e_b :<>: RunTimeArit (Lit 0))                             = rtZero
simplifyRunTime (True' :<>: runt)                                          = runt
simplifyRunTime (False' :<>: _)                                            = rtZero
simplifyRunTime (RunTimeArit (Lit 0) :++: runt)                            = runt
simplifyRunTime (runt :++: RunTimeArit (Lit 0))                            = runt
simplifyRunTime (_ :**: RunTimeArit (Lit 0))                               = rtZero
simplifyRunTime (1 :**: runt)                                              = runt
simplifyRunTime (0 :**: _)                                                 = rtZero
simplifyRunTime (k :**: RunTimeArit (Lit n))                               = rtLit (k*n)
simplifyRunTime otherwise                                                  = otherwise

-- Reglas recursivas para simplificar un RunTime
deepSimplifyRunTime :: RunTime -> RunTime
deepSimplifyRunTime (RunTimeArit arit) = RunTimeArit (completeNormArit arit)
deepSimplifyRunTime (bexp :<>: runt)   = simplifyRunTime (deepSimplifyBExp bexp :<>: deepSimplifyRunTime runt)
deepSimplifyRunTime (e_1 :++: e_2)     = simplifyRunTime (deepSimplifyRunTime e_1 :++: deepSimplifyRunTime e_2)
deepSimplifyRunTime (k :**: runt)      = simplifyRunTime (k :**: deepSimplifyRunTime runt)

---------------------------{ PREPARACIÓN PARA SBV }-------------------------------------------------

type Context = [BExp]

type Contexts = [Context]

type SolverInput = (Context, RArit, Names)

-- | Retorna todas las instancias de BExp dentro un RunTime sin repeticiones
findConditionRunTime :: RunTime -> Context
findConditionRunTime runt = rmdups conds
  where
    conds = f runt
    f (RunTimeArit _)        = []
    f ((Not bexp) :<>: runt) = bexp : findConditionRunTime runt
    -- @Fede: para mejorar la eficiencia, es preferible escribir bexp : (findConditionRunTime runt)
    f (bexp :<>: runt)       = bexp : findConditionRunTime runt
    f (e_1 :++: e_2)         = findConditionRunTime e_1 ++ findConditionRunTime e_2
    f (_ :**: runt)          = findConditionRunTime runt

-- | Toma un RunTime runt y retorna todos los posibles context (matriz de BExp) que
-- se pueden extraer a partir de los BExp que tiene el RunTime.
allContext :: RunTime -> Contexts
allContext runt = map (zipWith f conds) lbools
  where
    f bexp True = bexp
    f bexp _    = Not bexp
    conds       = findConditionRunTime runt
    lbools      = bools (length conds)

-- | Toma un BExp bexp y un RunTime runt, evalua todas las instancias de bexp
-- dentro de runt
evalCondition :: BExp -> RunTime -> RunTime
evalCondition bexp (RunTimeArit arit)     = RunTimeArit arit
evalCondition bexp1 (bexp2 :<>: runt)
  | bexp1 == bexp2                        = evalCondition bexp1 runt
  | deepSimplifyBExp (Not bexp1) == bexp2 = rtZero
  | otherwise                             = bexp2 :<>: evalCondition bexp1 runt
evalCondition bexp (e_1 :++: e_2)         = evalCondition bexp e_1 :++: evalCondition bexp e_2
evalCondition bexp (k :**: runt)          = k :**: evalCondition bexp runt

-- | Toma un RunTime runt y retorna su versión AExp en el caso de que se pueda
-- NOTA: En un principio pensé en usar la mónada maybe para que el porgrama no se caiga
-- pero en los ejemplos que vi siempre tiraban el error, así que quise seguir esa línea
runTimeToArit :: RunTime -> AExp
runTimeToArit (RunTimeArit arit) = arit
runTimeToArit (e_1 :++: e_2)     = runTimeToArit e_1 :+: runTimeToArit e_2
runTimeToArit (k :**: e)         = k :*: runTimeToArit e
runTimeToArit _                  = error $ "No hay versión directa a AExp" ++ show otherwise

-- Versión monádica de la función anterior
-- @Fede: me gusta esta forma monádica de escribirlo :)
runTimeToArit' :: RunTime -> Maybe AExp
runTimeToArit' (RunTimeArit arit) = Just arit
runTimeToArit' (e_1 :++: e_2) = do
  aexp1 <- runTimeToArit' e_1
  aexp2 <- runTimeToArit' e_2
  return (aexp1 :+: aexp2)
runTimeToArit' (k :**: e) = do
  aexp <- runTimeToArit' e
  return (k :*: aexp)
runTimeToArit' _ = Nothing

---------------------------------------------------------------------------------------------------
-- NOTA: Este algoritmo es poco claro y creo que debe cambiarse.
-- Descripción del algoritmo
-- 0 Entrega un arreglo de 3-tuplas (SolverInput
{- Cada 3-tupla representa problemas del tipo
        a <- sFloat "a"
        b <- sFloat "b"
        c <- sFloat "c"
        constrain $ a + 10.0 .< 19.0 + b
        constrain $ a + b + c.<= 10
        constrain $ Not (a + b<= 10)
-}
-- Cada tupla es
-- 0.a. Context: Un arreglo de BExp, es la hiṕotesis del implica
-- 0.a. Ejemplo [a + 10.0 .< 19.0 + b,  a + b + c.<= 10 ]
-- 0.b. Restricción: Restriction RArit, será la conclusión del Implica
-- 0.b. Ejemplo a + b<= 10
-- 0.c. Variables libres de todo el SolverInput
-- 0.c Ejemplo [a, b, c]

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
restrictionsToSolver rest = zip3 contexts eval_arit free_vars' -- 0
  where
    simplify_rest = fmap deepSimplifyRunTime rest -- 1
    contexts = allContext (foldRes (:++:) id simplify_rest) -- 2
    f = fmap . evalCondition -- 3
    eval_runt = map (foldr f simplify_rest) contexts -- 4
    eval_arit = map (fmap $ completeNormArit . runTimeToArit) eval_runt -- 5
    g = concatMap freeVarsBExp -- 6
    free_vars_bool = map g contexts -- 7
    free_vars_rest = map (foldRes (++) freeVars) eval_arit -- 8
    free_vars = map rmdups (zipWith (++) free_vars_bool free_vars_rest) -- 9
    free_vars' = map (filter (/= "")) free_vars -- 10
