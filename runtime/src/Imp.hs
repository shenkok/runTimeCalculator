module Imp where

import Data.Ratio
import GHC.Real (infinity)
import Data.SBV
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sortOn, sort)

{-
  MÓDULO QUE SE ENCARGA DE REPRESENTAR EXPRESIONES ARITMÉTICAS, BOOLEANAS, RUNTIMES Y PROGRAMAS
-}
type Name     = String -- Nombre de las variables

type Names    = [Name]

type Constant = AlgReal --  Constantes Numéricas

---------------------------------------- { FUNCIONES ÚTILES }---------------------------------------

-- | Elimina los elementos repetidos en una lista
rmdups :: (Eq a) => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- | Retorna una matriz con todas las posibles combinaciones False/True
-- de tamaño n.
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) r ++ map (True :) r
  where
    r = bools (n -1)

-- | Método para imprimir literales
-- | TODO: cambiar
showLit :: Rational -> String
showLit q
  | denominator q == 1 = show $ numerator q
  | otherwise          = show (numerator q) ++ "/" ++ show (denominator q)

---------------------------------------- { EXPRESIONES ARITMÉTICAS }--------------------------------

-- | Definición de Expresiones Aritméticas

data AExp
  = Lit Constant -- Números
  | Var Name -- Variables x, y, z
  | AExp :+: AExp --  Suma de expresiones aritméticas
  | AExp :*: AExp  -- Multipliacion de expresiones aritméticas
  deriving (Eq)

-- | Instancia Num para AExp: permite escribir literales enteros directamente
-- como AExp (p.ej. `2 :*: Var "x"`, donde `2` se resuelve via fromInteger)
-- y reusar (+)/(*)/negate en vez de tener que usar :+:/:*: a mano.
instance Num AExp where
  fromInteger n = Lit (fromInteger n)
  (+)           = (:+:)
  (*)           = (:*:)
  negate arit   = Lit (-1) :*: arit
  abs           = error "abs no está definido para AExp"
  signum        = error "signum no está definido para AExp"

-----------------------------------------{ AZÚCAR SINTÁCTICA}------------------------------------------------

-- | Azúcar sintáctica para la resta de expresiones aritméticas 
(-:) :: AExp -> AExp -> AExp
(-:) arit_1 arit_2 = arit_1 :+: (Lit (-1) :*: arit_2)

-------------------------------------------------------{DEFINICIÓN DE MONOMIOS Y POLINOMIOS}-------------------------------------------------

-- Un monomio: multiset de variables (nombre -> exponente)
type Monomial = Map Name Int

-- Un polinomio: monomio -> coeficiente
type Poly = Map Monomial Constant

---------------------------------------- { FUNCIONES EXPRESIONES ARITMÉTICAS }--------------------------------
-- | Función suma que simplifica el zero
(+:) :: AExp -> AExp -> AExp
(+:) (Lit 0) arit_1 = arit_1
(+:)  arit_1 (Lit 0) = arit_1
(+:) arit_1 arit_2 = arit_1 :+: arit_2


-- | Muestra un operando de :*: entre paréntesis salvo que sea atómico
-- (Lit/Var). Necesario porque :*: ya no está restringido a "constante * algo":
-- ambos lados pueden ser expresiones compuestas (ej. (x + y) * z), y sin esto
-- se pierde la asociación original al imprimir (mostraría "x + y*z").
showFactor :: AExp -> String
showFactor (Lit n) = show (Lit n)
showFactor (Var x) = show (Var x)
showFactor e        = "(" ++ show e ++ ")"

-- | Definición del método show para AExp
-- | TODO: Cambiar
instance Show AExp where
  show (Lit n)       = show n
  show (Var x)       = x
  show (e_1 :+: e_2) = show e_1 ++ " + " ++ show e_2
  show (e_1 :*: e_2) = showFactor e_1 ++ "*" ++ showFactor e_2

-- | Sustituye todas las instancias "x" en AritIn y por aritFor
sustAExp :: Name -> AExp -> AExp -> AExp
sustAExp _ _ (Lit n)             = Lit n
sustAExp x aritFor (Var y)       = if x == y then aritFor else Var y
sustAExp x aritFor (e_1 :+: e_2) = sustAExp x aritFor e_1 :+: sustAExp x aritFor e_2
sustAExp x aritFor (e_1 :*: e_2) = sustAExp x aritFor e_1 :*: sustAExp x aritFor e_2

-- | Toma un AExp arit y retorna una lista de todas las variables libres.
freeVars :: AExp -> Names
freeVars arit = sort (rmdups (fvar arit))
  where
    fvar (Lit _)       = []
    fvar (Var x)       = [x]
    fvar (e_1 :+: e_2) = fvar e_1 ++ fvar e_2
    fvar (e_1 :*: e_2) = fvar e_1 ++ fvar e_2

---------------------------------- { SIMPLIFICAR Y NORMALIZAR EXPRESIONES ARITMÉTICAS } -----------------------
-- Ahora que :*: es un producto genuino (AExp :*: AExp) y no sólo una ponderación
-- por constante, un peso lineal por variable (weightVar) ya no alcanza: no hay forma
-- de asignarle un "peso" válido a un término como x*y sin perder información. En su
-- lugar se normaliza expandiendo la expresión a un polinomio multivariado explícito
-- (Monomial = exponente por variable, Poly = coeficiente por monomio), que sí sabe
-- combinar términos semejantes también en el caso no lineal (x*y + y*x = 2*(x*y), x*x = x^2).

-- | Monomio de una única variable, x -> x^1
varMonomial :: Name -> Monomial
varMonomial x = Map.singleton x 1

-- | Monomio "vacío": representa el término constante (equivale a 1)
unitMonomial :: Monomial
unitMonomial = Map.empty

-- | Grado total de un monomio (suma de exponentes)
monomialDegree :: Monomial -> Int
monomialDegree = sum . Map.elems

-- | Producto de monomios: se suman los exponentes de cada variable
mulMonomial :: Monomial -> Monomial -> Monomial
mulMonomial = Map.unionWith (+)

-- | Poly de una constante (Map.empty codifica el 0, manteniendo el invariante
-- de que Poly nunca guarda coeficientes en 0)
constPoly :: Constant -> Poly
constPoly 0 = Map.empty
constPoly k = Map.singleton unitMonomial k

-- | Poly de una única variable
varPoly :: Name -> Poly
varPoly x = Map.singleton (varMonomial x) 1

-- | Suma de polinomios: suma coeficientes por monomio y descarta los que quedan en 0
addPoly :: Poly -> Poly -> Poly
addPoly p q = Map.filter (/= 0) (Map.unionWith (+) p q)

-- | Producto de polinomios: distribuye cada término de p sobre cada término de q
mulPoly :: Poly -> Poly -> Poly
mulPoly p q = Map.filter (/= 0) $ Map.fromListWith (+)
  [ (mulMonomial m1 m2, c1 * c2) | (m1, c1) <- Map.toList p, (m2, c2) <- Map.toList q ]

-- | Expande una expresión aritmética a su representación polinomial,
-- distribuyendo sumas y productos.
toPoly :: AExp -> Poly
toPoly (Lit n)     = constPoly n
toPoly (Var x)     = varPoly x
toPoly (e1 :+: e2) = addPoly (toPoly e1) (toPoly e2)
toPoly (e1 :*: e2) = mulPoly (toPoly e1) (toPoly e2)

-- | Reconstruye un monomio (sin su coeficiente) como AExp: {x:2, y:1} -> x*x*y.
-- No está definida para el monomio vacío, ese caso se maneja en termToAExp.
monomialToAExp :: Monomial -> AExp
monomialToAExp m = foldr1 (:*:) [ Var x | (x, e) <- Map.toAscList m, _ <- [1 .. e] ]

-- | Reconstruye un término (monomio, coeficiente) como AExp, omitiendo el
-- coeficiente cuando es 1 y el monomio no es el vacío.
termToAExp :: (Monomial, Constant) -> AExp
termToAExp (m, c)
  | Map.null m = Lit c
  | c == 1     = monomialToAExp m
  | otherwise  = Lit c :*: monomialToAExp m

-- | Orden canónico de términos: constante primero, luego por grado y,
-- a igual grado, alfabéticamente por variables. Así dos polinomios
-- iguales siempre se reconstruyen como el mismo AExp.
termOrder :: (Monomial, Constant) -> (Int, [(Name, Int)])
termOrder (m, _) = (monomialDegree m, Map.toAscList m)

-- | Reconstruye un AExp normalizado a partir de un polinomio.
fromPoly :: Poly -> AExp
fromPoly p = case sortOn termOrder (Map.toList p) of
  []       -> Lit 0
  (t : ts) -> foldl (\acc t' -> acc :+: termToAExp t') (termToAExp t) ts

-- | Normaliza una expresión aritmética a su forma polinomial canónica:
-- expande sumas/productos y combina términos semejantes (lineales y no lineales).
normArit :: AExp -> AExp
normArit = fromPoly . toPoly

-- | SimplifyArit hace una limpieza local (0 y 1 como neutros) sin expandir
-- productos de sumas, a diferencia de normArit. Útil como paso barato que no
-- reordena ni combina términos, sólo elimina redundancias obvias.
simplifyArit :: AExp -> AExp
simplifyArit (e_1 :+: e_2) = simplifyArit e_1 +: simplifyArit e_2
simplifyArit (e_1 :*: e_2) = mulSimplify (simplifyArit e_1) (simplifyArit e_2)
  where
    mulSimplify (Lit 0) _       = Lit 0
    mulSimplify _ (Lit 0)       = Lit 0
    mulSimplify (Lit 1) e       = e
    mulSimplify e (Lit 1)       = e
    mulSimplify e_1' e_2'       = e_1' :*: e_2'
simplifyArit arit = arit

-- | Retorna una versión normalizada de un AExp.
completeNormArit :: AExp -> AExp
completeNormArit = simplifyArit . normArit

-----------------------------------{ ALGUNAS EXPRESIONES ÚTILES}--------------------------------------

x :: AExp
x = Var "x"

y :: AExp 
y = Var "y"

one :: AExp
one = Lit 1

zero :: AExp
zero = Lit 0

-- infRational :: Rational 
-- infRational = infinity


---------------------------------- { EXPRESIONES BOOLEANAS} ------------------------------------------

-- | Definición de expresiones Booleanas
data BExp
  = True' -- Constante True
  | False' -- Constante False
  | AExp :<=: AExp -- Menor igual entre expresiones aritméticas
  | AExp :==: AExp -- Igualdad expresiones aritméticas
  | BExp :|: BExp -- Or lógico
  | BExp :&: BExp -- And Lógico
  | Not BExp -- Negación expresión booleana
  deriving (Eq) 
-- | Definición del método show para expresiones BExp.
instance Show BExp where
  show True'             = "true"
  show False'            = "false" 
  show ( e_1  :<=: e_2)  = show e_1 ++ " <= " ++ show e_2
  show ( e_1  :==: e_2)  = show e_1 ++ " == " ++ show e_2
  show ( e_1 :|: e_2)    = show e_1 ++ " || " ++ show e_2
  show (e_1 :&: e_2)     = show e_1 ++ " && " ++ show e_2
  show ( Not e)          = "!(" ++ show e ++ ")"
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

-- | Convierte booleanos en BExp
toBExp :: Bool -> BExp
toBExp True  = True'
toBExp False = False'

---------------------------------- { FUNCIONES EXPRESIONES BOOLEANAS } ------------------------------------------

-- | Función de sustitución toma una variable "x", un AExp aritFor y una expresión booleana AritIn
-- reemplaza todas las incidencias de "x" en la expresión aritIn por la expresión aritFor.
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
simplifyBExp (e_b1 :<=: e_b2)   = if e_b1 == e_b2 then True' else e_b1 :<=: e_b2
simplifyBExp (Lit q :==: Lit p) = toBExp (q == p)
simplifyBExp (e_b1 :==: e_b2)   = if e_b1 == e_b2 then True' else e_b1 :==: e_b2
simplifyBExp (True' :|: _)      = True'
simplifyBExp (_ :|: True')      = True'
simplifyBExp (e_b :|: False')   = e_b
simplifyBExp (False' :|: e_b)   = e_b
simplifyBExp (False' :&: _)     = False'
simplifyBExp (_ :&: False')     = False'
simplifyBExp (True' :&: e_b)    = e_b
simplifyBExp (e_b :&: True')    = e_b
simplifyBExp (Not (Not e_b))    = e_b
simplifyBExp (Not True')        = False'
simplifyBExp (Not False')       = True'
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
  | BExp :<>: RunTime -- Multiplicación por una condición
  | RunTime :++: RunTime -- Suma de RunTime
  | Constant :**: RunTime -- Ponderación por constante
  deriving (Eq) 

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

-- |Azúcar sintáctica para Función Indicatriz
toIndicator :: BExp -> RunTime
toIndicator e_b = e_b :<>: rtOne

-- | Azúcar sintáctica para operar una función indicatriz con expresión aritmética
(<>:) :: RunTime -> AExp -> RunTime
(<>:) (e_b :<>: (RunTimeArit (Lit 1))) arit = e_b :<>: RunTimeArit arit
(<>:) otherwise  _                          = error $ "El runtime no tiene la forma de indicatriz " ++ show otherwise
 ----------------------------------{ FUNCIONES RUNTIMES }-----------------------------------------------------

instance Show RunTime where
  show (RunTimeArit arit)               = show arit
  show (e_b :<>: RunTimeArit (Lit 1))   = "[" ++ show e_b ++ "]"
  show (e_b :<>: RunTimeArit (Lit n))   = "[" ++ show e_b ++ "]<>" ++ show n
  show (e_b :<>: RunTimeArit (Var x))   = "[" ++ show e_b ++ "]<>" ++ x
  show (e_b :<>: runt)                  = "[" ++ show e_b ++ "]<>" ++ "(" ++ show runt ++ ")"
  show (e_1 :++: e_2)                   = show e_1 ++ " ++ " ++ show e_2
  show (k :**: RunTimeArit (Lit n))     = show k ++ "**" ++ show n
  show (k :**: RunTimeArit (Var x))     = show k ++ "**" ++ x
  show (k :**: e)                       = show k ++ "**(" ++ show e ++ ")"

-- | Función de sustitución toma una variable "x", un AExp aritFor, un RunTime runtIn
-- reemplaza todas las incidencias de "x" en la expresión runtIn por la expresión aritFor.
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
simplifyRunTime (RunTimeArit arit_1 :++: (RunTimeArit arit_2 :++: runt))   = RunTimeArit (completeNormArit $ arit_1 :+: arit_2) :++: runt
simplifyRunTime (e_b :<>: RunTimeArit (Lit 0))                             = rtZero
simplifyRunTime (True' :<>: runt)                                          = runt
simplifyRunTime (False' :<>: _)                                            = rtZero
simplifyRunTime (RunTimeArit (Lit 0) :++: runt)                            = runt
simplifyRunTime (runt :++: RunTimeArit (Lit 0))                            = runt
simplifyRunTime (_ :**: RunTimeArit (Lit 0))                               = rtZero
simplifyRunTime (1 :**: runt)                                              = runt
simplifyRunTime (0 :**: _)                                                 = rtZero
simplifyRunTime (k :**: RunTimeArit arit)                                  = RunTimeArit $ completeNormArit (Lit k :*: arit)
simplifyRunTime otherwise                                                  = otherwise

-- Reglas recursivas para simplificar un RunTime
deepSimplifyRunTime :: RunTime -> RunTime
deepSimplifyRunTime (RunTimeArit arit) = RunTimeArit (completeNormArit arit)
deepSimplifyRunTime (bexp :<>: runt)   = simplifyRunTime (deepSimplifyBExp bexp :<>: deepSimplifyRunTime runt)
deepSimplifyRunTime (e_1 :++: e_2)     = simplifyRunTime (deepSimplifyRunTime e_1 :++: deepSimplifyRunTime e_2)
deepSimplifyRunTime (k :**: runt)      = simplifyRunTime (k :**: deepSimplifyRunTime runt)


----------------------------------{ CONSTRUCCIONES PROBABILISTAS} ----------------------------------
-- | Constante de distribuciones probabilísticas
type PConstant        = Constant

-- | Definición de una expresión probabilista singletón
type PBase a = (PConstant, a)

-- | Definición de una distribución de tipo a
type Distribution a   = [PBase a]

-- | Distribuciones útiles
type PAExp = Distribution AExp

----------------------------------{ AZÚCAR SINTÁCTICA}----------------------------------------------
-- | Azúcar sintáctica para generar una expresión aritmética
--  probabilista singular
(*~:) :: PConstant -> a -> Distribution a
(*~:) q a = [(q, a)]

-- | Azúcar sintáctica para la suma de expresiones aritméticas probabilistas 
(+~:) :: PAExp -> PAExp -> PAExp
(+~:) = (++)

-- | Método para imprimir un punto de la distribución 
showPoint :: (PConstant, AExp) -> String
showPoint (1, arit) = "<" ++ show arit ++ ">"
showPoint (q, arit) =  show q ++ "*<" ++ show arit ++ ">"


-- | Método para imprimir una distribución probabilista de expresiones aritméticas.
showPAexp :: PAExp -> String
showPAexp []       = ""
showPAexp (y:x:xs) = showPoint y ++ " + " ++ showPAexp (x:xs)
showPAexp (x: xs)  = showPoint x

-- | Expresiones Booleanas probabilistas // Muestras de distribuciones Bernoulli
newtype PBExp = Ber { p:: PConstant} deriving (Eq)

instance Show PBExp where
  show (Ber q) = "<" ++ show q ++ ">"

-- | Masa de una distribución  a
massDistribution :: Distribution a -> PConstant
massDistribution p_x = sum (map fst p_x)

-- | Comprueba que la suma de masa de probabilidad sea igual 1
isDistribution :: Distribution a -> Bool
isDistribution p_x = massDistribution p_x == 1 && and ps where
  predicate x = 0 <= x && x <= 1
  ps          = map (predicate . fst) p_x

----------------------------------------{AZÚCAR SINTÁCTICA PARA DISTRIBUCIONES CONOCIDAS}------------------------------
-- Muestra de distribución de Dirac
dirac :: AExp -> PAExp
dirac arit = [(1, arit)]

-- Muestra bernoulli de expresiones Aritméticas
coin :: PConstant -> PAExp
coin p = [(p, Lit 0), (1-p, Lit 1)]

-- Dado de N caras
-- uniform :: Constant -> Constant -> Distribution AExp
-- uniform a b = zip (repeat $ 1%len) values  where
--   values = map Lit [a..b]
--   len    = toInteger $ length values

-- Variable aleatoria con 1 como inicio o fin
-- uniform1 :: Constant -> Distribution AExp
-- uniform1 q  | q <= 1    = uniform q 1
--             | otherwise = uniform 1 q
----------------------------------------{AZÚCAR SINTÁCTICA PARA DISTRIBUCIONES CONOCIDAS}------------------------------

-- | Cálculo de esperanza.
-- Toma una distribución, una función de transformación de cada elemento,
-- una función de escala por probabilidad, una función de acumulación,
-- y un valor base para el fold.
expectedValue :: Distribution a -> (a -> b) -> (PConstant -> b -> c) -> (c -> d -> d) -> d -> d
expectedValue p_x transform scale combine base = foldr (combine . f) base p_x where
  f (k, e) = scale k (transform e)

-- | esperanza para distribuciones sobre expresiones aritméticas
aexpE :: Distribution AExp -> Name -> RunTime -> RunTime
aexpE p_x x runt = deepSimplifyRunTime $ expectedValue p_x f (:**:) (:++:) rtZero where
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
  = Skip                        -- Programa vacío que toma una unidad de tiempo
  | Empty                       -- Programa vacío sin costo de tiempo
  | Set Name AExp               -- Asignación
  | PSet Name PAExp             -- Asignación probabilista
  | Seq Program Program         -- Composición secuencial de programas
  | If BExp Program Program     -- Guarda condicional
  | PIf PBExp Program Program   -- Guarda condicional probabilista
  | While BExp Program RunTime  -- Ciclo while
  | PWhile PBExp Program RunTime -- Ciclo while probabilista
  deriving (Eq, Show)
-------------------------------------------{ FUNCIONES AUXILIARES }----------------------------------------------------------

-- | Función flip para usar en el while
flipw :: (a -> b -> c -> d) -> a -> c -> b -> d
flipw f b p runt = f b runt p

-------------------------------------------{ SIMPLIFICADOR PRoGRAMAS } ------------------------------------------------------

-- | Simplifica programas en 1 paso
simplifyProgram :: Program -> Program
simplifyProgram (Seq Empty program) = program
simplifyProgram (Seq program Empty) = program
simplifyProgram otherwise           = otherwise

-- | Simplificación recursiva de programas
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

-- | Azúcar sintáctica para el For
for :: Program -> Integer -> Program
for _       0 = Empty
for program 1 = program
for program n = Seq program (for program (n -1 ))
