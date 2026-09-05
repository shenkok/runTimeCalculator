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
  | AExp :*: AExp  -- Multiplicacion de expresiones aritméticas
  deriving (Eq, Ord) -- Ord: sólo se usa como clave de orden determinístico
                      -- (p.ej. para ordenar átomos de BExp en normBExp), no
                      -- tiene significado aritmético.

-- | Instancia Num para AExp: permite escribir literales enteros directamente
-- como AExp (p.ej. `2 :*: Var "x"`, donde `2` se resuelve via fromInteger)
-- y reusar (+)/(*)/negate en vez de tener que usar :+:/:*: a mano.
-- TODO: Normalizar cada vez que se opera
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
-- | TODO: Acá me tinca que falta un caso recursivo
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
  deriving (Eq, Ord) -- Ord: idem AExp, sólo orden determinístico para normBExp.

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

---------------------------{ FORMA NORMAL DE EXPRESIONES BOOLEANAS }-----------------------------------------------
-- normBExp da, para un BExp, un árbol canónico único up-to: reescritura
-- algebraica de los átomos, asociatividad/conmutatividad/idempotencia de
-- :&:/:|: y complementación (a && !a, a || !a). No calcula DNF/CNF (eso es
-- exponencial y es justo el blowup que se quiere evitar) — sólo fija una
-- única representación sintáctica para BExp que son estructuralmente
-- equivalentes salvo por cómo se escribieron.
--
-- Se hace en 3 pasadas, cada una respetando el invariante de la anterior:
-- 1. canonAtomsBExp: cada átomo `e1 <= e2` / `e1 == e2` pasa a "diferencia
--    contra cero" (`normArit (e1 - e2) <= 0` / `== 0`), así cualquier
--    reescritura algebraica del mismo átomo (x <= y, 0 <= y - x, x+1 <= y+1)
--    cae en el mismo AExp.
-- 2. toNNF: empuja Not hasta las hojas (De Morgan). Sobre reales, Not de un
--    átomo no se puede reescribir como otro átomo (Not (a<=b) no es b<=a,
--    sería a>b, distinto de a>=b en el punto de igualdad) así que Not sólo
--    puede terminar envolviendo un átomo ya canónico, nunca un :&:/:|:.
-- 3. normLogic: aplana cadenas anidadas de :&:/:|: en listas, ordena
--    (usando el Ord derivado, sólo como criterio determinístico) y
--    deduplica: así "a && b" y "b && a" (o "(a&&b)&&c" y "a&&(b&&c)")
--    terminan en el mismo árbol. De paso detecta complementación
--    (a && Not a = False', a || Not a = True').

-- | Lleva cada átomo (:<=:/:==:) a la forma canónica "diferencia contra
-- cero", recursivamente a través de :&:/:|:/Not.
canonAtomsBExp :: BExp -> BExp
canonAtomsBExp True'          = True'
canonAtomsBExp False'         = False'
canonAtomsBExp (e_1 :<=: e_2) = completeNormArit (e_1 -: e_2) :<=: Lit 0
canonAtomsBExp (e_1 :==: e_2) = completeNormArit (e_1 -: e_2) :==: Lit 0
canonAtomsBExp (e_1 :|: e_2)  = canonAtomsBExp e_1 :|: canonAtomsBExp e_2
canonAtomsBExp (e_1 :&: e_2)  = canonAtomsBExp e_1 :&: canonAtomsBExp e_2
canonAtomsBExp (Not e_b)      = Not (canonAtomsBExp e_b)

-- | Empuja Not hasta las hojas (forma normal de negación / De Morgan).
toNNF :: BExp -> BExp
toNNF True'                 = True'
toNNF False'                = False'
toNNF atom@(_ :<=: _)       = atom
toNNF atom@(_ :==: _)       = atom
toNNF (e_1 :|: e_2)         = toNNF e_1 :|: toNNF e_2
toNNF (e_1 :&: e_2)         = toNNF e_1 :&: toNNF e_2
toNNF (Not True')           = False'
toNNF (Not False')          = True'
toNNF (Not (Not e_b))       = toNNF e_b
toNNF (Not (e_1 :|: e_2))   = toNNF (Not e_1) :&: toNNF (Not e_2)
toNNF (Not (e_1 :&: e_2))   = toNNF (Not e_1) :|: toNNF (Not e_2)
toNNF (Not atom)            = Not atom -- atom :: _ :<=: _ | _ :==: _, ya canónico

-- | Complemento sintáctico de un literal ya en NNF (átomo o Not-átomo):
-- sólo alterna el Not de más afuera, nunca reescribe el átomo.
complementOf :: BExp -> BExp
complementOf (Not e_b) = e_b
complementOf e_b       = Not e_b

-- | Aplana una cadena de :&: (asociada de cualquier forma) en su lista de conjuntos.
flattenAnd :: BExp -> [BExp]
flattenAnd (e_1 :&: e_2) = flattenAnd e_1 ++ flattenAnd e_2
flattenAnd e_b           = [e_b]

-- | Aplana una cadena de :|: (asociada de cualquier forma) en su lista de disyuntos.
flattenOr :: BExp -> [BExp]
flattenOr (e_1 :|: e_2) = flattenOr e_1 ++ flattenOr e_2
flattenOr e_b           = [e_b]

-- | Reconstruye una conjunción a partir de una lista de literales ya
-- normalizados: ordena, deduplica, y absorbe False'/complementación.
-- TODO: REvisar media mágica
buildAnd :: [BExp] -> BExp
buildAnd lits
  | False' `elem` cleaned          = False'
  | any hasComplement cleaned      = False'
  | null cleaned                   = True'
  | otherwise                      = foldr1 (:&:) cleaned
  where
    cleaned = sort (rmdups (filter (/= True') lits))
    hasComplement l = complementOf l `elem` cleaned

-- | Idem buildAnd, dual para disyunciones (True' absorbe, se filtra False').
-- TODO: REvisar media mágica
buildOr :: [BExp] -> BExp
buildOr lits
  | True' `elem` cleaned           = True'
  | any hasComplement cleaned      = True'
  | null cleaned                   = False'
  | otherwise                      = foldr1 (:|:) cleaned
  where
    cleaned = sort (rmdups (filter (/= False') lits))
    hasComplement l = complementOf l `elem` cleaned

-- | Aplana, ordena y deduplica :&:/:|: recursivamente. Asume que ya se
-- corrió toNNF (Not sólo envuelve átomos).
normLogic :: BExp -> BExp
normLogic True'           = True'
normLogic False'          = False'
normLogic atom@(_ :<=: _) = atom
normLogic atom@(_ :==: _) = atom
normLogic (Not e_b)       = Not (normLogic e_b)
normLogic e_b@(_ :&: _)   = buildAnd (map normLogic (flattenAnd e_b))
normLogic e_b@(_ :|: _)   = buildOr (map normLogic (flattenOr e_b))

-- | Forma normal de una expresión booleana: ver comentario arriba.
normBExp :: BExp -> BExp
normBExp = normLogic . toNNF . canonAtomsBExp
----------------------------------{ RUNTIMES }-----------------------------------------------------
-- | Definición de RunTimes
data RunTime
  = RunTimeArit AExp -- RunTime hecho a partir de una expresión aritmética
  |RunTimeBExp BExp -- Runtime hecho a partir de un BExp,, genera una indicatriz
  | RunTime :++: RunTime -- Suma de RunTime
  | RunTime :**: RunTime -- Multiplicación entre RunTimes
  deriving (Eq)

-- | Instancia Num para RunTime: permite escribir literales enteros
-- directamente como RunTime (p.ej. `2 :**: rtVar "x"`, donde `2` se resuelve
-- vía fromInteger), igual que hace `instance Num AExp` para AExp. Necesaria
-- porque, al eliminarse la ponderación por constante (antes `Constant :**:
-- RunTime`), la única forma de escribir un peso literal es como un
-- `RunTime` en sí (`RunTimeArit (Lit k)`).
-- Cuidado: igual que en AExp, negate/(-1) se construye directo con
-- `RunTimeArit (Lit (-1)) :**: runt`, nunca con `(-1) :**: runt` dentro de
-- la propia instancia — causaría recursión infinita en negate.
instance Num RunTime where
  fromInteger n = RunTimeArit (Lit (fromInteger n))
  (+)           = (:++:)
  (*)           = (:**:)
  negate runt   = RunTimeArit (Lit (-1)) :**: runt
  abs           = error "abs no está definido para RunTime"
  signum        = error "signum no está definido para RunTime"

----------------------------------{ AZÚCAR SINTÁCTICA } -----------------------------------------------------

-- | Azúcar sintáctica para el menos
(--:) :: RunTime -> RunTime -> RunTime
runt_1 --: runt_2 = runt_1 :++: (RunTimeArit (Lit (-1)) :**: runt_2)

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

-- |Azúcar sintáctica para Función Indicatriz: RunTimeBExp e_b YA es la
-- indicatriz (vale 0 o 1), no hace falta ponderarla por rtOne como antes.
toIndicator :: BExp -> RunTime
toIndicator e_b = RunTimeBExp e_b

 ----------------------------------{ FUNCIONES RUNTIMES }-----------------------------------------------------

-- | Muestra un operando de :**: entre paréntesis salvo que sea atómico
-- (RunTimeArit/RunTimeBExp, que ya llevan su propio delimitador). Análogo a
-- showFactor para AExp :*:, necesario porque :**: ya no está restringido a
-- "constante ** algo": ambos lados pueden ser RunTime compuestos.
showRTFactor :: RunTime -> String
showRTFactor r@(RunTimeArit _) = show r
showRTFactor r@(RunTimeBExp _) = show r
showRTFactor r                 = "(" ++ show r ++ ")"

instance Show RunTime where
  show (RunTimeArit arit) = show arit
  show (RunTimeBExp e_b)  = "[" ++ show e_b ++ "]"
  show (e_1 :++: e_2)     = show e_1 ++ " ++ " ++ show e_2
  show (r_1 :**: r_2)     = showRTFactor r_1 ++ "**" ++ showRTFactor r_2

-- | Función de sustitución toma una variable "x", un AExp aritFor, un RunTime runtIn
-- reemplaza todas las incidencias de "x" en la expresión runtIn por la expresión aritFor.
sustRunTime :: Name -> AExp -> RunTime -> RunTime
sustRunTime x aritFor (RunTimeArit aritIn) = RunTimeArit (sustAExp x aritFor aritIn)
sustRunTime x aritFor (RunTimeBExp bexp)   = RunTimeBExp (sustBExp x aritFor bexp)
sustRunTime x aritFor (e_1 :++: e_2)       = sustRunTime x aritFor e_1 :++: sustRunTime x aritFor e_2
sustRunTime x aritFor (r_1 :**: r_2)       = sustRunTime x aritFor r_1 :**: sustRunTime x aritFor r_2

-- | Entrega las variables libres dentro de un Runtime
freeVarsRunTime :: RunTime -> Names
freeVarsRunTime (RunTimeArit arit) = freeVars arit
freeVarsRunTime (RunTimeBExp b)    = freeVarsBExp b
freeVarsRunTime (e_1 :++: e_2)     = freeVarsRunTime e_1 ++ freeVarsRunTime e_2
freeVarsRunTime (r_1 :**: r_2)     = freeVarsRunTime r_1 ++ freeVarsRunTime r_2

-------------------- {  SIMPLIFICAR RUNTIMES }------------------------------------------------------------------
-- TODO: releer toda la siplificación

-- | Aplana una cadena de :**: (asociada de cualquier forma, izquierda o
-- derecha, mezclada) en la lista de sus factores. Necesario porque, a
-- diferencia de :++: (que sólo se reasocia hacia la derecha y por lo tanto
-- puede fusionarse par a par sin riesgo), :**: no tiene una única dirección
-- "correcta" — una indicatriz ahora es un factor más, no un caso aparte, y
-- reasociar por pares en ambas direcciones a la vez puede oscilar
-- indefinidamente cuando el par que toca fusionar primero no reduce a nada
-- (se probó a mano: intentarlo cuelga el simplificador). Aplanar toda la
-- cadena de una vez y reconstruirla con buildMul es la única forma segura
-- de canonicalizar un producto sin importar cómo haya quedado parentizado.
flattenMul :: RunTime -> [RunTime]
flattenMul (r_1 :**: r_2) = flattenMul r_1 ++ flattenMul r_2
flattenMul r              = [r]

-- | Reconstruye un producto ya canonicalizado a partir de su lista de
-- factores (ver flattenMul): junta todas las indicatrices (RunTimeBExp) en
-- una única conjunción a la izquierda, todos los pesos aritméticos
-- (RunTimeArit) en un único polinomio a la derecha (vía completeNormArit),
-- y deja cualquier otro factor (ej. una suma :++: usada como peso) tal
-- cual, multiplicando al final en el orden en que aparecía. Maneja los
-- casos absorbentes (una indicatriz que colapsa a False', o un peso que
-- colapsa a 0) devolviendo rtZero directo, y omite cualquier parte que
-- colapse al neutro (True'/Lit 1) del resultado final — si TODO colapsa al
-- neutro (producto vacío), el resultado es rtOne.
buildMul :: [RunTime] -> RunTime
buildMul factors
  | boolPart == Just False'  = rtZero
  | aritPart == Just (Lit 0) = rtZero
  | otherwise                = case boolFactor ++ aritFactor ++ others of
      []       -> rtOne
      (f : fs) -> foldr1 (:**:) (f : fs)
  where
    bools    = [b | RunTimeBExp b <- factors]
    arits    = [a | RunTimeArit a <- factors]
    others   = filter isOther factors
      where
        isOther (RunTimeBExp _) = False
        isOther (RunTimeArit _) = False
        isOther _               = True
    boolPart = if null bools then Nothing else Just (foldr1 (\b1 b2 -> simplifyBExp (b1 :&: b2)) bools)
    aritPart = if null arits then Nothing else Just (completeNormArit (foldr1 (:*:) arits))
    boolFactor = case boolPart of
      Just True' -> []
      Just b     -> [RunTimeBExp b]
      Nothing    -> []
    aritFactor = case aritPart of
      Just (Lit 1) -> []
      Just a       -> [RunTimeArit a]
      Nothing      -> []

-- | Si el RunTime es una indicatriz (con o sin peso explícito), retorna su
-- condición y su peso: "RunTimeBExp b :**: r" da (b, r), y una indicatriz
-- desnuda "RunTimeBExp b" (peso implícito 1 — la forma en la que queda tras
-- pasar por buildMul cuando el peso colapsa al neutro, ej. "[b]**1") da
-- (b, rtOne). Nothing para cualquier otro RunTime. Necesaria para que las
-- reglas 1/1b de abajo reconozcan una indicatriz sin importar si buildMul
-- ya le quitó el "**1" redundante.
asIndicatorWeight :: RunTime -> Maybe (BExp, RunTime)
asIndicatorWeight (RunTimeBExp b :**: r) = Just (b, r)
asIndicatorWeight (RunTimeBExp b)        = Just (b, rtOne)
asIndicatorWeight _                      = Nothing

-- | Reglas de un sólo paso para simplificar un RunTime
--
-- Además de las reglas algebraicas originales (neutros de :++:/:**:,
-- literales), se agregaron reglas puntuales para casos que antes quedaban
-- sin combinar aunque fueran sintácticamente adyacentes (no es una forma
-- normal completa de RunTime — eso sigue pendiente, ver CLAUDE.md — sólo
-- cierra los casos más obvios). Una indicatriz ponderada, antes un
-- constructor dedicado (b :<>: r), ahora es sólo "RunTimeBExp b :**: r" —
-- multiplicación genuina entre dos RunTime — así que las reglas de abajo se
-- expresan sobre ese patrón:
--
--   1. [b]**r1 ++ [b]**r2  =  [b]**(r1 ++ r2), cuando la condición "b" es
--      sintácticamente igual (Eq derivado de BExp) en ambos sumandos, tanto
--      si son los dos únicos términos como si el segundo encabeza una
--      cadena de :++: más larga. Se reconoce una indicatriz vía
--      asIndicatorWeight, así que también aplica cuando uno de los lados
--      quedó como indicatriz desnuda (peso 1 implícito, ej. tras pasar por
--      buildMul).
--   1b. [b]**r ++ [!b]**r = r: la suma de indicatrices complementarias con
--      el mismo peso "r" es 1*r. A diferencia de la regla 1, esto NO se
--      generaliza a "suma de indicatrices = OR" — [b1]+[b2] = [b1 || b2]
--      sólo vale si b1/b2 son mutuamente excluyentes, y en general no lo
--      son (si ambas son verdaderas, la suma da 2, no 1). El único caso
--      donde la exclusión mutua es gratis (verdadera para cualquier b) es
--      justo el complemento sintáctico b/¬b, que es lo que se detecta acá
--      vía complementOf (misma función que usa normBExp).
--   2. Reasociación de :++: hacia la derecha: (e1 ++ e2) ++ e3 = e1 ++ (e2 ++ e3).
--      Sirve para que las reglas 1/1b (y las reglas de RunTimeArit ya
--      existentes, que asumen la cadena asociada a la derecha) encuentren
--      términos adyacentes aunque el árbol original los haya parentizado a
--      la izquierda.
--   3. Cualquier :**: (con cualquier forma de anidamiento) se canonicaliza
--      de una vez vía flattenMul/buildMul: fusiona todos los pesos
--      aritméticos en un polinomio único, todas las indicatrices en una
--      única conjunción, y aplica los neutros/absorbentes (Lit 1/Lit 0,
--      True'/False') sobre el resultado ya combinado. Esto reemplaza (y
--      generaliza a cualquier nivel de anidamiento, no sólo el
--      inmediatamente adyacente) lo que antes eran varias reglas sueltas
--      para :**: — incluida la vieja "b1 :<>: (b2 :<>: r) = (b1:&:b2):<>:r",
--      que queda subsumida acá.
simplifyRunTime :: RunTime -> RunTime
simplifyRunTime (RunTimeArit arit_1 :++: RunTimeArit arit_2)               = RunTimeArit $ completeNormArit (arit_1 :+: arit_2)
simplifyRunTime (RunTimeArit arit_1 :++: (RunTimeArit arit_2 :++: runt))   = RunTimeArit (completeNormArit $ arit_1 :+: arit_2) :++: runt
simplifyRunTime (t_1 :++: t_2)
  | Just (e_b1, r_1) <- asIndicatorWeight t_1
  , Just (e_b2, r_2) <- asIndicatorWeight t_2
  , e_b1 == e_b2                             = simplifyRunTime (RunTimeBExp e_b1 :**: simplifyRunTime (r_1 :++: r_2))
  | Just (e_b1, r_1) <- asIndicatorWeight t_1
  , Just (e_b2, r_2) <- asIndicatorWeight t_2
  , complementOf e_b1 == e_b2 && r_1 == r_2  = r_1
simplifyRunTime (t_1 :++: (t_2 :++: runt))
  | Just (e_b1, r_1) <- asIndicatorWeight t_1
  , Just (e_b2, r_2) <- asIndicatorWeight t_2
  , e_b1 == e_b2                             = simplifyRunTime (simplifyRunTime (RunTimeBExp e_b1 :**: simplifyRunTime (r_1 :++: r_2)) :++: runt)
  | Just (e_b1, r_1) <- asIndicatorWeight t_1
  , Just (e_b2, r_2) <- asIndicatorWeight t_2
  , complementOf e_b1 == e_b2 && r_1 == r_2  = simplifyRunTime (r_1 :++: runt)
simplifyRunTime ((e_1 :++: e_2) :++: e_3)                                  = simplifyRunTime (e_1 :++: simplifyRunTime (e_2 :++: e_3))
simplifyRunTime (RunTimeArit (Lit 0) :++: runt)                            = runt
simplifyRunTime (runt :++: RunTimeArit (Lit 0))                            = runt
simplifyRunTime rt@(_ :**: _)                                              = buildMul (flattenMul rt)
simplifyRunTime otherwise                                                  = otherwise

-- Reglas recursivas para simplificar un RunTime
deepSimplifyRunTime :: RunTime -> RunTime
deepSimplifyRunTime (RunTimeArit arit) = RunTimeArit (completeNormArit arit)
deepSimplifyRunTime (RunTimeBExp bexp) = RunTimeBExp (deepSimplifyBExp bexp)
deepSimplifyRunTime (e_1 :++: e_2)     = simplifyRunTime (deepSimplifyRunTime e_1 :++: deepSimplifyRunTime e_2)
deepSimplifyRunTime (r_1 :**: r_2)     = simplifyRunTime (deepSimplifyRunTime r_1 :**: deepSimplifyRunTime r_2)


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

-- | Variables libres de una distribución de expresiones aritméticas: la
-- unión (sin repetidos) de las variables libres de cada punto de la
-- distribución. Usada por ImpVCGen.getExistencialAndUniversalVars para que
-- una asignación probabilista (PSet x parit) aporte también las variables
-- que aparecen dentro de "parit" (ej. `x :~ <y>`), no sólo "x".
freeVarsPAExp :: PAExp -> Names
freeVarsPAExp p_x = sort (rmdups (concatMap (freeVars . snd) p_x))

----------------------------------------{AZÚCAR SINTÁCTICA PARA DISTRIBUCIONES CONOCIDAS}------------------------------
-- Muestra de distribución de Dirac
dirac :: AExp -> PAExp
dirac arit = [(1, arit)]

-- Muestra bernoulli de expresiones Aritméticas
coin :: PConstant -> PAExp
coin p = [(p, Lit 0), (1-p, Lit 1)]

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
aexpE p_x x runt = deepSimplifyRunTime $ expectedValue p_x f scale (:++:) rtZero where
  f arit = sustRunTime x arit runt
  -- scale pondera por la probabilidad "k": ya no hay Constant :**: RunTime,
  -- así que el literal se envuelve como RunTime antes de multiplicar.
  scale k r = RunTimeArit (Lit k) :**: r


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
