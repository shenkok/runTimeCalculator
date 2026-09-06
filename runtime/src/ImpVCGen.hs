module ImpVCGen where

import Imp
import Data.Set (Set)
import qualified Data.Set as Set

{-
    MODULO QUE SE ENCARGA DE GENERAR EL INPUT PARA SBV A PARTIR
    DE LOS LENGUAJES IMPERATIVOS 
-}
-----------------------------------{RESTRICCIONES }-----------------------------------------------------

-- | Definición de restricción
-- | Ya uso el :==: y :<=: en AExp. Por lo mismo añado un igual (=) más.
data Restriction a
  = a :<==: a
  deriving (Eq, Show)

-- | Extender a Functor
instance Functor Restriction where
  -- fmap :: (a -> b) -> Restriction a -> Restriction b
  fmap f (a_1 :<==: a_2) = f a_1 :<==: f a_2

-- | Definición de una función de fold para la estructura Restriction
foldRes :: (b -> b -> c) -> (a -> b) -> Restriction a -> c
foldRes f g (e_1 :<==: e_2) = f (g e_1) (g e_2)

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
vcGenerator (If e_b e_t e_f) runt    = (rtOne :++: ((RunTimeBExp e_b :**: fst vc_t) :++: (RunTimeBExp (Not e_b) :**: fst vc_f)), snd vc_t ++ snd vc_f)
  where
    vc_t = vcGenerator e_t runt
    vc_f = vcGenerator e_f runt
vcGenerator (PIf pe_b e_t e_f) runt  = (rtOne :++: ((rtLit p_true :**: fst vc_t) :++: (rtLit p_false :**: fst vc_f)), snd vc_t ++ snd vc_f)
  where
    p_true = p pe_b
    p_false = 1 - p_true
    vc_t = vcGenerator e_t runt
    vc_f = vcGenerator e_f runt
vcGenerator (Seq p_1 p_2) runt       = (fst vc_1, snd vc_1 ++ snd vc_2)
  where
    vc_2 = vcGenerator p_2 runt
    vc_1 = vcGenerator p_1 (fst vc_2)
vcGenerator (While e_b p inv) runt   = (inv, (l_inv :<==: inv) : snd vc_p)
  where
    vc_p = vcGenerator p inv
    l_inv = rtOne :++: ((RunTimeBExp (Not e_b) :**: runt) :++: (RunTimeBExp e_b :**: fst vc_p))
vcGenerator (PWhile pe_b c inv) runt = (inv, (l_inv :<==: inv) : snd vc_p)
  where
    p_true = p pe_b
    p_false = 1 - p_true
    vc_p = vcGenerator c inv
    l_inv = rtOne :++: ((rtLit p_false :**: runt) :++: (rtLit p_true :**: fst vc_p))


-- | Genera las restricciones considerando al 0 como runtime
vcGenerator0 :: Program -> (RunTime, [RRunTime])
vcGenerator0 program = vcGenerator program rtZero


------------------------------------------{VCGENERATOR V 2.0}------------------------------------------------------------

freeVarsProgram :: Program -> Names
freeVarsProgram Skip                = []
freeVarsProgram Empty               = []
freeVarsProgram (Set x arit)        = x: freeVars arit
freeVarsProgram (PSet x parit)      = [x]
freeVarsProgram (If e_b e_t e_f)    = freeVarsBExp e_b ++ freeVarsProgram e_t ++ freeVarsProgram e_f
freeVarsProgram (PIf _ e_t e_f)     = freeVarsProgram e_t ++ freeVarsProgram e_f
freeVarsProgram (Seq p_1 p_2)       = freeVarsProgram p_1 ++ freeVarsProgram p_2
freeVarsProgram (While e_b p _)   = freeVarsBExp e_b ++ freeVarsProgram p
freeVarsProgram (PWhile _ c _)     = freeVarsProgram c



-- Data para guardar lo información relevante de un programa y su respectivo VCGen
-- runtime: Es el tiempo de ejecución calculado del programa
-- restrictions: Son las restricciones que se generan a partir del programa
-- programVariables: Son las variables que aparecen en el programa
-- invariantVariables: Son las variables que aparecen en los invariantes del programa

data RestrictionInformation = RestrictionInformation
  {
    restriction :: RRunTime,
    templateVars:: Names,
    programVars :: Names
  } deriving (Eq, Show)


data ProgramVCGenInformation = ProgramVCGenInformation
  {
    runtime :: RunTime,
    restrictionsInformation :: [RestrictionInformation]
  } deriving (Eq, Show)

onlyInFirst :: Eq a => [a] -> [a] -> [a]
onlyInFirst xs ys = rmdups (filter (`notElem` ys) xs)

-- TODO: verificar si existen varailes libres e los parit 
-- TODO: Testear
vcGenerator' :: Program -> RunTime -> ProgramVCGenInformation
vcGenerator' Skip runt                = ProgramVCGenInformation (rtOne :++: runt) []
vcGenerator' Empty runt               = ProgramVCGenInformation runt [] 
vcGenerator' (Set x arit) runt        = ProgramVCGenInformation (rtOne :++: sustRunTime x arit runt) []
vcGenerator' (PSet x parit) runt      = ProgramVCGenInformation (rtOne :++: aexpE parit x runt) []
vcGenerator' (If e_b e_t e_f) runt    = ProgramVCGenInformation (rtOne :++: ((RunTimeBExp e_b :**: runtime vc_t) :++: (RunTimeBExp (Not e_b) :**: runtime vc_f))) (restrictionsInformation vc_t ++ restrictionsInformation vc_f)
  where
    vc_t = vcGenerator' e_t runt
    vc_f = vcGenerator' e_f runt
vcGenerator' (PIf pe_b e_t e_f) runt  = ProgramVCGenInformation (rtOne :++: ((rtLit p_true :**: runtime vc_t) :++: (rtLit p_false :**: runtime vc_f))) (restrictionsInformation vc_t ++ restrictionsInformation vc_f)
  where
    p_true = p pe_b
    p_false = 1 - p_true
    vc_t = vcGenerator' e_t runt
    vc_f = vcGenerator' e_f runt
vcGenerator' (Seq p_1 p_2) runt       = ProgramVCGenInformation (runtime vc_1) (restrictionsInformation vc_1 ++ restrictionsInformation vc_2)
  where
    vc_2 = vcGenerator' p_2 runt
    vc_1 = vcGenerator' p_1 (runtime vc_2)
vcGenerator' (While e_b p inv) runt   = ProgramVCGenInformation inv (RestrictionInformation (l_inv :<==: inv) template_vars program_vars : restrictionsInformation vc_p)
  where
    vc_p = vcGenerator' p inv
    l_inv = rtOne :++: ((RunTimeBExp (Not e_b) :**: runt) :++: (RunTimeBExp e_b :**: runtime vc_p))
    program_vars = freeVarsProgram p
    template_vars = onlyInFirst (freeVarsRunTime inv ++ freeVarsRunTime l_inv) program_vars
vcGenerator' (PWhile pe_b c inv) runt = ProgramVCGenInformation inv (RestrictionInformation (l_inv :<==: inv) template_vars program_vars : restrictionsInformation vc_p)
  where
    p_true = p pe_b
    p_false = 1 - p_true
    vc_p = vcGenerator' c inv
    l_inv = rtOne :++: ((rtLit p_false :**: runt) :++: (rtLit p_true :**: runtime vc_p))
    program_vars = freeVarsProgram c
    template_vars = onlyInFirst (freeVarsRunTime inv ++ freeVarsRunTime l_inv) program_vars

----------------------------------{ OMEGA - CPO TÓPICOS}-------------------------------------------------

bottom :: RunTime
bottom = rtLit 0

-- top :: RunTime
-- top = rtLit $ toRational (1/0)

-- | Función característica de un while
cfWhile :: BExp -> Program -> RunTime -> RunTime -> RunTime
cfWhile b program runt x = rtOne :++: ((RunTimeBExp (Not b) :**: runt) :++: (RunTimeBExp b :**: fst (vcGenerator program x)))

-- | Función característica de un pwhile
cfPWhile :: PBExp -> Program -> RunTime -> RunTime ->  RunTime
cfPWhile (Ber p) program runt x = rtOne :++: ((rtLit (1 - p) :**: runt) :++: (rtLit p :**: fst (vcGenerator program x)))

-- | Iteración de punto fijo para un while
fpWhile :: RunTime -> BExp -> Program -> RunTime -> Int -> RunTime
fpWhile x b program runt 0 = x
fpWhile x b program runt n = cfw (fpWhile x b program runt (n - 1)) where
  cfw = cfWhile b program runt

-- | Iteración de punto fijo para un pwhile
fpPWhile :: RunTime -> PBExp -> Program -> RunTime -> Int -> RunTime
fpPWhile x ber program runt 0 = x
fpPWhile x ber program runt n = cfpw (fpPWhile x ber program runt (n - 1)) where
  cfpw = cfPWhile ber program runt

-----------------------------------------------{PREPARACIÓN PARA SBV} -----------------------------------------

type Context = [BExp]

type Contexts = [Context]

type SolverInput = (Context, RArit, Names)


-- Nueva versión de SolverInput
-- Ahora modela problemas del tipo
-- a <- sRational "a"
-- b <- forall sRational "b"
-- constraint $ (a + 10 .>0 .& b .==6 .=> a + b .<= 10) .& (a + 10 .>0 .& b .==6 .=> a + b .<= 10)
-- Nótese que ahora no se hace el juego algebreico de tomar los implicar y reemplazarlos por un and lógicos.
-- Se modela directamente como una conjunción de implicaciones.
-- Por cada posible contexto se genera una implicación. Por eso solver_formulaes es un arreglo de implicaciones.

data Implication = Implication{
  hypothesis :: Context,
  conclusion :: BExp } deriving (Eq, Show)

-- | existential/for_all van como Set Name (no Names/[Name]) para que
-- duplicados sintácticos (la misma variable libre apareciendo en más de un
-- punto del programa/invariante, ej. en varios contextos de un while
-- anidado) no terminen repetidos en la lista que se le pasa a sReals /
-- mkUniversales.
data SolverInput' = SolverInput'
  { solver_formulaes ::[Implication]
  , existential :: Set Name
  , for_all      :: Set Name
   } deriving (Eq, Show)

-- | Retorna todas las instancias de BExp dentro un RunTime sin repeticiones
--
-- La indicatriz "RunTimeBExp bexp" es ahora una hoja (antes era un
-- constructor de dos campos "bexp :<>: runt"): el caso de multiplicación de
-- abajo (f (r_1 :**: r_2) = getBExp r_1 ++ getBExp r_2) ya recorre ambos
-- lados de cualquier :**:, así que sólo hace falta declarar qué BExp aporta
-- la hoja misma — no una regla combinada para "indicatriz seguida de peso".
-- Se preserva el detalle de que, si la condición es una negación (Not
-- bexp), se guarda el bexp sin negar (igual que antes), para que
-- allContext siempre trabaje con la forma "positiva" de cada condición.
getBExp :: RunTime -> Context
getBExp runt = rmdups conds
  where
    conds = f runt
    f (RunTimeArit _)          = []
    f (RunTimeBExp (Not bexp)) = [bexp]
    f (RunTimeBExp bexp)       = [bexp]
    f (e_1 :++: e_2)           = getBExp e_1 ++ getBExp e_2
    f (r_1 :**: r_2)           = getBExp r_1 ++ getBExp r_2

-- | Toma un RunTime runt y retorna todos los posibles context (matriz de BExp) que
-- se pueden extraer a partir de los BExp que tiene el RunTime.
-- Para el RunTime 1 ++ [x >= 0] ++ [w < 0] se extraen los contextos
-- [[!(0.0 <= x),!(0.0 <= w)],
-- [!(0.0 <= x),   0.0 <= w],
-- [0.0 <= x,    !(0.0 <= w)],
-- [0.0 <= x,      0.0 <= w]]
allContext :: RunTime -> Contexts
allContext runt = map (zipWith f conds) lbools
  where
    f bexp True = bexp
    f bexp _    = Not bexp
    conds       = getBExp runt
    lbools      = bools (length conds)

-- | Toma un BExp bexp y un RunTime runt, evalua todas las instancias de bexp
-- dentro de runt
--
-- La indicatriz ponderada "bexp2 :<>: runt" de antes es ahora
-- "RunTimeBExp bexp2 :**: runt" (multiplicación genuina): se agrega un caso
-- explícito para esa forma (en vez de dejar que la recursión genérica de
-- :**: la parta en dos llamadas independientes) para reproducir exactamente
-- el comportamiento viejo — en particular, cuando bexp1 coincide con la
-- condición de la indicatriz, el resultado es directamente
-- "evalCondition bexp1 runt" (la indicatriz desaparece, no queda como
-- "1 :**: ..."), y cuando es su complemento el resultado colapsa a rtZero
-- de una, sin dejar un "0 :**: ..." pendiente de simplificar.
evalCondition :: BExp -> RunTime -> RunTime
evalCondition bexp (RunTimeArit arit)     = RunTimeArit arit
evalCondition bexp1 (RunTimeBExp bexp2 :**: runt)
  | bexp1 == bexp2                        = evalCondition bexp1 runt
  | deepSimplifyBExp (Not bexp1) == bexp2 = rtZero
  | otherwise                             = RunTimeBExp bexp2 :**: evalCondition bexp1 runt
evalCondition bexp1 (RunTimeBExp bexp2)
  | bexp1 == bexp2                        = rtOne
  | deepSimplifyBExp (Not bexp1) == bexp2 = rtZero
  | otherwise                             = RunTimeBExp bexp2
evalCondition bexp (e_1 :++: e_2)         = evalCondition bexp e_1 :++: evalCondition bexp e_2
evalCondition bexp (r_1 :**: r_2)         = evalCondition bexp r_1 :**: evalCondition bexp r_2

-- | Toma un RunTime runt y retorna su versión AExp en el caso de que se pueda

runTimeToArit :: RunTime -> AExp
runTimeToArit (RunTimeArit arit) = arit
runTimeToArit (e_1 :++: e_2)     = runTimeToArit e_1 :+: runTimeToArit e_2
runTimeToArit (r_1 :**: r_2)     = runTimeToArit r_1 :*: runTimeToArit r_2
runTimeToArit  otherwise         = error $ "No hay versión directa a AExp" ++ show otherwise

-- Versión monádica de la función anterior
runTimeToArit' :: RunTime -> Maybe AExp
runTimeToArit' (RunTimeArit arit) = Just arit
runTimeToArit' (e_1 :++: e_2) = do
  aexp1 <- runTimeToArit' e_1
  aexp2 <- runTimeToArit' e_2
  return (aexp1 :+: aexp2)
runTimeToArit' (r_1 :**: r_2) = do
  aexp1 <- runTimeToArit' r_1
  aexp2 <- runTimeToArit' r_2
  return (aexp1 :*: aexp2)
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
-- 0.a. Context: Un arreglo de BExp, es la hipótesis del implica
-- 0.a. Ejemplo [a + 10.0 .< 19.0 + b,  a + b + c.<= 10 ]
-- 0.b. Restricción: Restriction RArit, será la conclusión del Implica
-- 0.b. Ejemplo a + b<= 10
-- 0.c. Variables libres de todo el SolverInput
-- 0.c Ejemplo [a, b, c]

-- 1. Simplificar los dos runtimes de la restricción a:!<=:b -> a':!<=:b'
-- 2. Extraer todos los contextos posibles de los dos runtime a' y b',
-- sumo los runtime porque es una forma de tomar ambos contextos de una vez.
--  TODO: Es algo artificial sumas ambos runtimes, mejor pasar las restricion a:<=:b to
--  a-b:<=:0 y tomar los contextos de a-b.
-- 3. Función currificada, para poder evaluar una condición y usarla con los contexts
-- 4. Evaluar todos los contextos y generar todas las posibles restricciones de runtimes [a'':!<=:b'' ]
-- 5. A partir de las restricciones de runtimes [a'':<=:b''], se extrae la expresión aritmética subyacente
--    generando restricciones de AExp [a''' :!<=b'''].
-- 6. Función que toma un context [BExp] y extrae las variables libres y las concatena
-- 7. Arreglo cuyos elementos son arreglos con todas las variables libres de cada posible contexto
-- 8. Variables libres de cada restricción de AExp [a''' :!<=b''']
-- 9. Uno las variables libres de cada context con las variables de su respectiva restricción
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


-- | A partir de una restricción de RunTime genero las formulas lógicas que tomará SBV
-- TODO: Por ahora el manejo de las variables no lo decido acá, ya que no he decido un standar para
-- reconocer una variable existencial de una universal.
-- Estas fórmulas tiene la forma de
-- condición_1_1 /\ condición_1_2 /\ ... /\ condición_1_n => restricción_1
-- condición_2_1 /\ condición_2_2 /\ ... /\ condición_2_n => restricción_2
-- && ...
-- condición_m_1 /\ condición_m_2 /\ ... /\ condición_m_n => restricción_m
-- 1. Simplifico la restricción, para evitar pasos innecesarios paso primero de a:!<=:b -> a - b:!<=:0 -> c:!<=:0
-- 2. Extraigo todos los posibles contextos del runtime c
-- 3. Defino una función que retorne las restricción_i definas arriba, a partir de cada contexto_i
-- 3a. El algoritmo que sigo es ir envaluando cada condición_i_j sobre el runtime c, y al final me queda un runtime c_i.
-- 3b. Esta runtime c_i lo transformo a AExp y lo comparo 0, para que quede de la forma c_i:!<=:0 y este último término
-- es la restricción_i que busco.
-- 4. Dado que ya puedo obtener la restricción_i a partir de cada contexto_i, aplico la función f a todos los contextos.
-- 4a. El resultado final es un arreglo de implicaciones donde la hipotesis del implica_i es el contexto_i y la conclusión es la restricción_i.

restrictionsToImplications :: RRunTime -> [Implication]
restrictionsToImplications (runtimeA :<==: runtimeB) = map (\x -> Implication { hypothesis = x, conclusion = f x }) contexts -- 4
  where
    simplify_runtime = deepSimplifyRunTime runtimeA --: runtimeB  -- 1
    contexts = allContext simplify_runtime                        -- 2
    f = (:<=: zero). runTimeToArit . foldr evalCondition simplify_runtime -- 3

-- TODO: definir función que retorne las variables universales y existenciales.
-- TODO: Verificar si PAEXP tiene variables libres
-- TODO: veri si existen varaibles lñibres e los pif

-- data Program
--   = Skip                        -- Programa vacío que toma una unidad de tiempo
--   | Empty                       -- Programa vacío sin costo de tiempo
--   | Set Name AExp               -- Asignación
--   | PSet Name PAExp             -- Asignación probabilista
--   | Seq Program Program         -- Composición secuencial de programas
--   | If BExp Program Program     -- Guarda condicional
--   | PIf PBExp Program Program   -- Guarda condicional probabilista
--   | While BExp Program RunTime  -- Ciclo while
--   | PWhile PBExp Program RunTime -- Ciclo while probabilista
--   deriving (Eq, Show)
-- ----------------------

-- NOTA (pendiente, no resuelto acá): cuando hay varios `while`/`pwhile`
-- anidados, cada invariante aporta sus variables libres a la misma lista
-- "exist_variables" de más abajo, sin distinguir a cuál invariante
-- pertenece cada una. Mientras el usuario use nombres de template distintos
-- entre sí (y distintos de las variables del programa) esto no causa
-- problema — cada nombre sigue identificando sin ambigüedad a una única
-- variable existencial — pero la función no tiene forma de, por ejemplo,
-- agrupar "qué variables existenciales pertenecen a qué invariante" si
-- hiciera falta más adelante (para reportar mejor un contraejemplo, o para
-- resolver cada invariante por separado en vez de todos juntos).
--
-- TODO (pendiente, discutido 2026-09-03, no resuelto acá): la heurística de
-- esta función es puramente sintáctica — "universal" si el nombre aparece
-- asignado o en una guarda real en algún lugar del programa, "existencial"
-- si no. Ya considera tanto la condición del while/pwhile (freeVarsBExp e_b)
-- como el cuerpo (recursión sobre get_variables p), así que "mirar la
-- condición" no es el gap. El caso real que rompe esto es un ciclo cuya
-- condición es una constante (ej. `while(false){...}`) y cuyo cuerpo nunca
-- toca la variable en cuestión — ahí NI la condición NI el cuerpo aportan
-- ninguna señal sobre esa variable, y queda indistinguible de una variable
-- de template genuina (que tampoco aparece nunca asignada ni en guardas, por
-- diseño). Caso concreto confirmado: `cdvcMenos` en ImpProgram.hs
-- (`while(false){inv=[x>=0]<>x}{skip}`, = Cdvc- del informe, Anexo C.1.4,
-- pp. 73-74) — "x" queda existencial, y programToSolverInputs responde
-- "válido" con testigo x=1 en vez de "no válido" con contraejemplo x=-1/x=0
-- (que es lo que el informe prueba a mano para ese mismo programa). Test que
-- documenta el hallazgo: test/ImpProgramSpec.hs, describe "caso donde la
-- heurística de variables existenciales confunde al modo nuevo". Ver
-- CLAUDE.md, sección "getExistencialAndUniversalVars / SolverInput'".
getExistencialAndUniversalVars :: Program -> (Names, Names)
getExistencialAndUniversalVars program = (onlyInFirst exist_variables universal_variables, universal_variables)
  where
    get_variables :: Program -> (Names, Names)
    get_variables Skip                = ([], [])
    get_variables Empty               = ([], [])
    get_variables (Set x arit)        = ([], x:freeVars arit)
    get_variables (PSet x parit)      = ([], x:freeVarsPAExp parit)
    get_variables (If e_b e_t e_f)    = (fst true_variables ++ fst false_variables, freeVarsBExp e_b ++ snd true_variables ++ snd false_variables)
          where
            true_variables  = get_variables e_t
            false_variables = get_variables e_f
    get_variables (PIf _ e_t e_f)  = (fst true_variables ++ fst false_variables, snd true_variables ++ snd false_variables)
          where
            true_variables  = get_variables e_t
            false_variables = get_variables e_f
    get_variables (Seq p_1 p_2)       = (fst true_variables ++ fst false_variables, snd true_variables ++ snd false_variables)
          where
            true_variables = get_variables p_1
            false_variables = get_variables p_2
    get_variables (While e_b p inv)   = (freeVarsRunTime inv ++ fst body_variables, freeVarsBExp e_b ++ snd body_variables)
          where
            body_variables = get_variables p
    get_variables (PWhile _ c inv) =  (freeVarsRunTime inv ++ fst body_variables, snd body_variables)
          where
            body_variables = get_variables c
    (exist_variables, universal_variables) = get_variables program


-- | Todos los invariantes de ciclo de un programa, en el MISMO orden en que
-- vcGenerator recorre el programa — o sea, en el mismo orden en que aparecen
-- sus restricciones dentro de la lista que devuelve vcGenerator0 (cada
-- while/pwhile aporta exactamente una restricción, la suya, antes que las de
-- su cuerpo). Esa correspondencia 1-a-1 es la que usa programToSolverInputs
-- para pegarle a cada obligación la restricción de buena-definición de *su*
-- invariante; si alguna vez cambia el orden de recorrido de vcGenerator, hay
-- que actualizar esta función en paralelo.
programInvariants :: Program -> [RunTime]
programInvariants Skip             = []
programInvariants Empty            = []
programInvariants (Set _ _)        = []
programInvariants (PSet _ _)       = []
programInvariants (Seq p_1 p_2)    = programInvariants p_1 ++ programInvariants p_2
programInvariants (If _ e_t e_f)   = programInvariants e_t ++ programInvariants e_f
programInvariants (PIf _ e_t e_f)  = programInvariants e_t ++ programInvariants e_f
programInvariants (While _ p inv)  = inv : programInvariants p
programInvariants (PWhile _ c inv) = inv : programInvariants c

-- | Restricción de buena-definición ("well-definedness") de un invariante.
--
-- Un RunTime representa un tiempo de ejecución *esperado*, así que no puede
-- ser negativo en ningún estado. La condición de inductividad por sí sola
-- (Φ(I) ≤ I) no lo garantiza: sin esta restricción, Z3 puede devolver como
-- "testigo" de un invariante-plantilla valores negativos (observado con los
-- dos invariantes anidados de Cpvc, donde salían coeficientes como -10/9),
-- que satisfacen la desigualdad pero no representan ningún tiempo de
-- ejecución. Es la misma condición que Batz et al. (TACAS 2023) listan como
-- parte de la admisibilidad de un invariante, junto a inductividad y
-- seguridad.
wellDefinedness :: RunTime -> RRunTime
wellDefinedness inv = rtZero :<==: inv

programToSolverInput :: Program -> SolverInput'
programToSolverInput program = SolverInput' { solver_formulaes = concatMap restrictionsToImplications (rest ++ well_defined)
                                             , existential = Set.fromList exist_vars
                                             , for_all = Set.fromList universal_vars }
  where
    (exist_vars, universal_vars) = getExistencialAndUniversalVars program
    (runt, rest) = vcGenerator0 program
    well_defined = map wellDefinedness (programInvariants program)

-- | Variables libres de una implicación: unión de las de la hipótesis (el
-- contexto, una lista de BExp) y las de la conclusión.
freeVarsImplication :: Implication -> Names
freeVarsImplication (Implication hyp concl) = concatMap freeVarsBExp hyp ++ freeVarsBExp concl

-- | Filtra "vars" (ya clasificadas como existenciales/universales para todo
-- el programa por getExistencialAndUniversalVars) a sólo las que
-- efectivamente aparecen en "formulaes". Sirve para que cada SolverInput'
-- individual de programToSolverInputs no arrastre variables de otros
-- invariantes que no le pertenecen (evita, por ejemplo, acercarse más rápido
-- de lo necesario al tope de 3 variables universales de mkUniversales).
relevantVars :: [Implication] -> Names -> Set Name
relevantVars formulaes vars = Set.fromList vars `Set.intersection` Set.fromList used
  where used = concatMap freeVarsImplication formulaes

-- | Igual que programToSolverInput, pero entrega un SolverInput' *por cada*
-- restricción del programa (una por cada while/pwhile) en vez de juntarlas
-- todas en un solo problema — así cada invariante se puede resolver (y
-- reportar) como un problema de SBV independiente.
--
-- No se etiqueta explícitamente a qué invariante corresponde cada elemento
-- más allá del orden de la lista: es el mismo orden en que vcGenerator
-- recorre el programa (ver el comentario sobre invariantes anidados en
-- getExistencialAndUniversalVars) — alcanza para imprimir "para el
-- invariante 1 se generan estas restricciones, para el invariante 2 estas
-- otras, etc." enumerando la lista, pero no para identificar el while/pwhile
-- de origen por nombre o ubicación si hiciera falta más adelante.
-- ATENCIÓN: resolver estos SolverInput' por separado sólo es correcto cuando
-- no hay variables de template compartidas entre dos obligaciones. Si las
-- hay (típico en ciclos anidados, donde el invariante externo depende del
-- interno), cada problema elige sus propios valores para esas variables y
-- pueden salir testigos contradictorios entre obligaciones — todas
-- "válidas" sin que exista un único invariante que las cumpla a la vez. Para
-- ese caso hay que usar programToSolverInput (singular), que las junta en un
-- solo problema con existenciales compartidos. ImpIO.completeRoutine' hace
-- esa elección automáticamente vía ImpIO.sharedExistentials.
programToSolverInputs :: Program -> [SolverInput']
programToSolverInputs program = map toSolverInput (zip rest well_defined)
  where
    (exist_vars, universal_vars) = getExistencialAndUniversalVars program
    (_, rest) = vcGenerator0 program
    well_defined = map wellDefinedness (programInvariants program)
    toSolverInput (restriction, well_defined_restriction) = SolverInput'
      { solver_formulaes = formulaes
      , existential      = relevantVars formulaes exist_vars
      , for_all          = relevantVars formulaes universal_vars
      }
      where formulaes = restrictionsToImplications restriction
                     ++ restrictionsToImplications well_defined_restriction