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

showLit :: Rational -> String
showLit q 
  | denominator q == 1 = show $ numerator q
  | otherwise        = show (numerator q) ++ "/" ++ show (denominator q)

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
  deriving (Eq)  -- Ponderación por una constante

-----------------------------------------{ AZÚCAR SINTÁCTICA}------------------------------------------------
(-:) :: AExp -> AExp -> AExp
arit_1 -: arit_2 = arit_1 :+: ((-1) :*: arit_2) 

---------------------------------------- { FUNCIONES EXPRESIONES ARITMÉTICAS }--------------------------------
-- | Definición del método show para AExp
instance Show AExp where
  show (Lit n)           = showLit n
  show (Var x)           = show x
  show (e_1 :+: e_2)     = show e_1  ++ " + " ++ show e_2
  show (k :*: Lit n)     = showLit k ++  "*"  ++ showLit n
  show (k :*: Var x)     = showLit k ++  "*"  ++ show x 
  show (k :*: e_2)       = showLit k ++  "*(" ++ show e_2 ++")"
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
simplifyArit (Lit 0 :+: arit)    = simplifyArit arit
simplifyArit (arit  :+: Lit 0)   = simplifyArit arit
simplifyArit (arit_1 :+: arit_2) = simplifyArit arit_1 :+: simplifyArit arit_2
simplifyArit (1 :*: arit)        = simplifyArit arit
simplifyArit (0 :*: _)           = Lit 0
simplifyArit (_ :*: Lit 0)       = Lit 0
simplifyArit (k :*: arit)        = k :*: simplifyArit arit
simplifyArit otherwise           = otherwise

-- | Retorna una versión normalizada de un AExp.
completeNormArit :: AExp -> AExp
completeNormArit = simplifyArit . normArit

---------------------------------- { EXPRESIONES BOOLEANAS} ------------------------------------------
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

-- | Convierte boolealos en BExp
toBExp :: Bool -> BExp
toBExp True  = True'
toBExp False = False'
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

---------------------------{ SIMPLIFICAR EXPRESIONES BOOLEANAS }---------------------------------------------------------

-- | Reglas de un sólo paso para simplificar un BExp
simplifyBExp :: BExp -> BExp
simplifyBExp (Lit q :<=: Lit p) = toBExp (q <= p)
simplifyBExp (e_b1 :<=: e_b2)   = if (e_b1 == e_b2) then True' else (e_b1 :<=: e_b2)
simplifyBExp (Lit q :==: Lit p) = toBExp (q == p)
simplifyBExp (e_b1 :==: e_b2)   = if (e_b1 == e_b2) then True' else (e_b1 :==: e_b2)
simplifyBExp (True' :|: _)      = True'
simplifyBExp (_ :|: True')      = True'
simplifyBExp (e_b :|: False')   = e_b
simplifyBExp (False' :|: e_b)   = e_b
simplifyBExp (False' :&: _)     = False'
simplifyBExp (_ :&: False')     = False'
simplifyBExp (True' :&: e_b)    = e_b
simplifyBExp (e_b :&: True')    = e_b
simplifyBExp (Not (Not e_b))    = e_b
simplifyBExp (Not (True'))      = False'
simplifyBExp (Not(False'))      = True'
simplifyBExp otherwise          = otherwise

-- | Reglas recursivas para simplificar un BExp
deepSimplifyBExp :: BExp -> BExp
deepSimplifyBExp True'          = True'
deepSimplifyBExp False'         = False'
deepSimplifyBExp (e_1 :<=: e_2) = simplifyBExp (completeNormArit e_1 :<=: completeNormArit e_2)
deepSimplifyBExp (e_1 :==: e_2) = simplifyBExp (completeNormArit e_1 :==: completeNormArit e_2)
deepSimplifyBExp (e_1 :|: e_2)  = simplifyBExp (deepSimplifyBExp e_1 :|: deepSimplifyBExp e_2)
deepSimplifyBExp (e_1 :&: e_2)  = simplifyBExp (deepSimplifyBExp e_1 :&: deepSimplifyBExp e_2)
deepSimplifyBExp (Not e_b)      = simplifyBExp (Not $ deepSimplifyBExp e_b)
----------------------------------{ RUNTIMES }-----------------------------------------------------
-- | Definición de RunTimes
data RunTime
  = RunTimeArit AExp -- RunTime hecho a partir de una expresión aritmética
  | BExp :<>: RunTime -- multiplicación por una condición
  | RunTime :++: RunTime -- suma de RunTime
  | Constant :**: RunTime
  deriving (Eq) -- ponderación por constante

----------------------------------{ AZÚCAR SINTÁCTICA } -----------------------------------------------------

-- | Azúcar sintáctica para el menos
(--:) :: RunTime -> RunTime -> RunTime
runt_1 --: runt_2 = runt_1 :++: ((-1) :**: runt_2)

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

-- |Azúcar sintpactica para Función Indicatriz 
toIndicator :: BExp -> RunTime
toIndicator e_b = e_b :<>: rtOne

-- | Azúcar sintáctica para operar una función indicatriz con expresión aritmética
(<>:) :: RunTime -> AExp -> RunTime
(e_b :<>: (RunTimeArit (Lit 1))) <>: arit = e_b :<>: (RunTimeArit arit) 
otherwise <>: _                           = error $ "El runtime no tiene la forma de indicatriz " ++ show otherwise
 ----------------------------------{ FUNCIONES RUNTIMES }-----------------------------------------------------

instance Show RunTime where                                         
  show (RunTimeArit arit)               = show arit
  show (e_b :<>: RunTimeArit (Lit 1))   = "[" ++ show e_b ++ "]"
  show (e_b :<>: RunTimeArit (Lit n))   = "[" ++ show e_b ++ "]<>" ++ showLit n
  show (e_b :<>: RunTimeArit (Var x))   = "[" ++ show e_b ++ "]<>" ++ show x
  show (e_b :<>: runt)                  = "[" ++ show e_b ++ "]<>" ++ "(" ++ show runt ++ ")"
  show (e_1 :++: e_2)                   = show e_1 ++ " ++ " ++ show e_2
  show (k :**: RunTimeArit (Lit n))     = showLit k ++ "**" ++ showLit n
  show (k :**: RunTimeArit (Var x))     = showLit k ++ "**" ++ show k
  show (k :**: e_2)                     = showLit k ++ "**(" ++ show e_2 ++ ")"

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

-------------------- {  SIMPLIFICAR RUNTIMES }------------------------------------------------------------------

-- | Reglas de un sólo paso para simplificar un RunTime
simplifyRunTime :: RunTime -> RunTime
simplifyRunTime (RunTimeArit arit_1 :++: RunTimeArit arit_2)               = RunTimeArit $ completeNormArit (arit_1 :+: arit_2)
simplifyRunTime (RunTimeArit arit_1 :++: (RunTimeArit arit_2 :++: runt))   = (RunTimeArit $ completeNormArit $ arit_1 :+: arit_2) :++: runt
simplifyRunTime (e_b :<>: RunTimeArit (Lit 0))                             = rtZero
simplifyRunTime (True' :<>: runt)                                          = runt
simplifyRunTime (False' :<>: _)                                            = rtZero
simplifyRunTime (RunTimeArit (Lit 0) :++: runt)                            = runt
simplifyRunTime (runt :++: RunTimeArit (Lit 0))                            = runt
simplifyRunTime (_ :**: RunTimeArit (Lit 0))                               = rtZero
simplifyRunTime (1 :**: runt)                                              = runt
simplifyRunTime (0 :**: _)                                                 = rtZero
simplifyRunTime (k :**: RunTimeArit arit)                                  = RunTimeArit $ completeNormArit (k:*:arit)
simplifyRunTime otherwise                                                  = otherwise

-- Reglas recursivas para simplificar un RunTime
deepSimplifyRunTime :: RunTime -> RunTime
deepSimplifyRunTime (RunTimeArit arit) = RunTimeArit (completeNormArit arit)
deepSimplifyRunTime (bexp :<>: runt)   = simplifyRunTime (deepSimplifyBExp bexp :<>: deepSimplifyRunTime runt)
deepSimplifyRunTime (e_1 :++: e_2)     = simplifyRunTime (deepSimplifyRunTime e_1 :++: deepSimplifyRunTime e_2)
deepSimplifyRunTime (k :**: runt)      = simplifyRunTime (k :**: deepSimplifyRunTime runt)


----------------------------------{ CONSTRUCCIONES PROBABILISTAS} ----------------------------------
-- | Constante de dsitribuciones probabilisticas
type PConstant        = Constant

-- | Definición de una expresión porbabilista singletón
type PBase a = (PConstant, a)

-- | Definición de una distribución de tipo a
type Distribution a   = [PBase a]

-- | Distribuciones útiles
type PAExp = Distribution AExp

----------------------------------{ AZÚCAR SINTÁCTICA}----------------------------------------------
-- | Azúcar sintáctica para generar una expresión aritmética
--  probalilista singular
(*~:) :: PConstant -> a -> Distribution a
(*~:) q a = [(q, a)]

(+~:) :: PAExp -> PAExp -> PAExp
(+~:) = (++)

-- | Método para mostar un punto de la distribución 
showPoint :: (PConstant, AExp) -> String
showPoint (1, arit) = "<" ++ show arit ++ ">" 
showPoint (q, arit) =  showLit q ++ "*<" ++ show arit ++ ">"   

showPAexp :: PAExp -> String
showPAexp []       = ""
showPAexp (y:x:xs) = showPoint y ++ " + "
showPAexp (x: xs)  = showPoint x

newtype PBExp = Ber { p:: PConstant} deriving (Eq)
  
instance Show PBExp where
  show (Ber q) = "<" ++ showLit q ++ ">"

-- | Masa de una distribución  a
massDistribution :: Distribution a -> PConstant
massDistribution p_x = foldr (+) 0 (fst.unzip $ p_x)

-- | Comprueba que la suma de masa de probabilidad sea igual 1
isDistribution :: Distribution a -> Bool
isDistribution p_x = (massDistribution p_x == 1) && foldr (&&) True ps where
  predicate x = (0 <= x) && (x <= 1)
  ps          = map (predicate . fst) p_x

----------------------------------------{AZÚCAR SINTÁCTICA PARA DISTRIBUCIONES CONOCIDAS}------------------------------
-- Muestra de dsitribución de Dirac
dirac :: AExp -> PAExp
dirac arit = [(1, arit)]

-- Muestra bernoulli de expresiones Ariméticas
coin :: PConstant -> PAExp
coin p = [(p, Lit 0), (1-p, Lit 1)]

-- Dado de N caras
uniform :: Constant -> Constant -> Distribution AExp
uniform a b = zip (repeat $ 1%len) values  where
  values = map Lit [a..b]
  len = toInteger $ length values

-- Variable aleatoria con 1 como inicio o fin
uniform1 :: Constant -> Distribution AExp
uniform1 q  | q <= 1 = uniform q 1
            | otherwise = uniform 1 q
----------------------------------------{AZÚCAR SINTÁCTICA PARA DISTRIBUCIONES CONOCIDAS}------------------------------

-- | Calculo de esperanza. toma una distribución, una función de transformacion para un a
--, una función * que retorna un c, una función + que retorna un d, un caso base para el fold y retorna un tipo d
expectation :: Distribution a -> (a -> b) -> (PConstant -> b -> c)-> (c -> d -> d) -> d -> d
expectation p_x h prod sum base =  foldr sum base (map f p_x) where
  f (k, e) = prod k (h  e)

-- | esperanza para dsitribuciones sobre expresiones aritméticas
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
  | While BExp Program RunTime -- ciclo while probabilista
  | PWhile PBExp Program RunTime
  deriving (Eq, Show) -- ciclo while
-------------------------------------------{ FUNCIONES AUXILIARES }----------------------------------------------------------
-- | Función flip para usar en el while
flipw :: (a -> b -> c -> d) -> a -> c -> b -> d
flipw f b p runt = f b runt p
-------------------------------------------{ SIMPLIFICADOR PORGRAMAS } ------------------------------------------------------
simplifyProgram :: Program -> Program
simplifyProgram (Seq Empty program) = program
simplifyProgram otherwise           = otherwise

deepSimplifyProgram :: Program -> Program
deepSimplifyProgram Skip                           = Skip
deepSimplifyProgram Empty                          = Empty
deepSimplifyProgram (Set name arit)                = Set name arit
deepSimplifyProgram (PSet name parit)              = PSet name parit
deepSimplifyProgram (Seq program_1 program_2)      = simplifyProgram (Seq (deepSimplifyProgram program_1) (deepSimplifyProgram program_2))
deepSimplifyProgram (If e_b program_1 program_2)   = If e_b (deepSimplifyProgram program_1) (deepSimplifyProgram program_2)
deepSimplifyProgram (PIf pe_b program_1 program_2) = PIf pe_b (deepSimplifyProgram program_1) (deepSimplifyProgram program_2)
deepSimplifyProgram (While e_b program runt)       = While e_b (deepSimplifyProgram program) runt
deepSimplifyProgram (PWhile pe_b program runt)     = PWhile pe_b (deepSimplifyProgram program) runt

----------------------------------{ AZÚCAR SINTÁCTICA PARA PROGRAMAS }-----------------------------------------------------

-- | Azúcar sintáctica para If con Empty en la rama false y un programa en la rama True
it :: BExp -> Program -> Program
it b program = If b program Empty

-- | Azúcar sintáctica para PIf con Empty en la rama false y un programa en la rama True
pit :: PBExp -> Program -> Program
pit ber program = PIf ber program Empty