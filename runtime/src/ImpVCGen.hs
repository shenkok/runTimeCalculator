module ImpVCGen where

import Imp

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
vcGenerator (If e_b e_t e_f) runt    = (rtOne :++: ((e_b :<>: fst vc_t) :++: (Not e_b :<>: fst vc_f)), snd vc_t ++ snd vc_f)
  where
    vc_t = vcGenerator e_t runt
    vc_f = vcGenerator e_f runt
vcGenerator (PIf pe_b e_t e_f) runt  = (rtOne :++: ((p_true :**: fst vc_t) :++: (p_false :**: fst vc_f)), snd vc_t ++ snd vc_f)
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
    l_inv = rtOne :++: ((Not e_b :<>: runt) :++: (e_b :<>: fst vc_p))
vcGenerator (PWhile pe_b c inv) runt = (inv, (l_inv :<==: inv) : snd vc_p)
  where
    p_true = p pe_b
    p_false = 1 - p_true
    vc_p = vcGenerator c inv
    l_inv = rtOne :++: ((p_false :**: runt) :++: (p_true :**: fst vc_p))


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
vcGenerator' (If e_b e_t e_f) runt    = ProgramVCGenInformation (rtOne :++: ((e_b :<>: runtime vc_t) :++: (Not e_b :<>: runtime vc_f))) (restrictionsInformation vc_t ++ restrictionsInformation vc_f) 
  where
    vc_t = vcGenerator' e_t runt
    vc_f = vcGenerator' e_f runt
vcGenerator' (PIf pe_b e_t e_f) runt  = ProgramVCGenInformation (rtOne :++: ((p_true :**: runtime vc_t) :++: (p_false :**: runtime vc_f))) (restrictionsInformation vc_t ++ restrictionsInformation vc_f)
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
    l_inv = rtOne :++: ((Not e_b :<>: runt) :++: (e_b :<>: runtime vc_p))
    program_vars = freeVarsProgram p
    template_vars = onlyInFirst (freeVarsRunTime inv ++ freeVarsRunTime l_inv) program_vars
vcGenerator' (PWhile pe_b c inv) runt = ProgramVCGenInformation inv (RestrictionInformation (l_inv :<==: inv) template_vars program_vars : restrictionsInformation vc_p)
  where
    p_true = p pe_b
    p_false = 1 - p_true
    vc_p = vcGenerator' c inv
    l_inv = rtOne :++: ((p_false :**: runt) :++: (p_true :**: runtime vc_p))
    program_vars = freeVarsProgram c
    template_vars = onlyInFirst (freeVarsRunTime inv ++ freeVarsRunTime l_inv) program_vars

----------------------------------{ OMEGA - CPO TÓPICOS}-------------------------------------------------

bottom :: RunTime
bottom = rtLit 0

-- top :: RunTime
-- top = rtLit $ toRational (1/0)

-- | Función característica de un while
cfWhile :: BExp -> Program -> RunTime -> RunTime -> RunTime
cfWhile b program runt x = rtOne :++: ((Not b :<>: runt) :++: (b :<>: fst (vcGenerator program x)))

-- | Función característica de un pwhile
cfPWhile :: PBExp -> Program -> RunTime -> RunTime ->  RunTime
cfPWhile (Ber p) program runt x = rtOne :++: (((1 - p) :**: runt) :++: (p :**: fst (vcGenerator program x)))

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

data SolverInput' = SolverInput'
  { solver_formulaes ::[Implication]
  , existential :: Names
  , for_all      :: Names
   } deriving (Eq, Show)

-- | Retorna todas las instancias de BExp dentro un RunTime sin repeticiones
getBExp :: RunTime -> Context
getBExp runt = rmdups conds
  where
    conds = f runt
    f (RunTimeArit _)        = []
    f ((Not bexp) :<>: runt) = bexp : getBExp runt
    f (bexp :<>: runt)       = bexp : getBExp runt
    f (e_1 :++: e_2)         = getBExp e_1 ++ getBExp e_2
    f (_ :**: runt)          = getBExp runt

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
evalCondition :: BExp -> RunTime -> RunTime
evalCondition bexp (RunTimeArit arit)     = RunTimeArit arit
evalCondition bexp1 (bexp2 :<>: runt)
  | bexp1 == bexp2                        = evalCondition bexp1 runt
  | deepSimplifyBExp (Not bexp1) == bexp2 = rtZero
  | otherwise                             = bexp2 :<>: evalCondition bexp1 runt
evalCondition bexp (e_1 :++: e_2)         = evalCondition bexp e_1 :++: evalCondition bexp e_2
evalCondition bexp (k :**: runt)          = k :**: evalCondition bexp runt

-- | Toma un RunTime runt y retorna su versión AExp en el caso de que se pueda

runTimeToArit :: RunTime -> AExp
runTimeToArit (RunTimeArit arit) = arit
runTimeToArit (e_1 :++: e_2)     = runTimeToArit e_1 :+: runTimeToArit e_2
runTimeToArit (k :**: e)         = Lit k :*: runTimeToArit e
runTimeToArit  otherwise         = error $ "No hay versión directa a AExp" ++ show otherwise

-- Versión monádica de la función anterior
runTimeToArit' :: RunTime -> Maybe AExp
runTimeToArit' (RunTimeArit arit) = Just arit
runTimeToArit' (e_1 :++: e_2) = do
  aexp1 <- runTimeToArit' e_1
  aexp2 <- runTimeToArit' e_2
  return (aexp1 :+: aexp2)
runTimeToArit' (k :**: e) = do
  aexp <- runTimeToArit' e
  return (Lit k :*: aexp)
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

getExistencialAndUniversalVars :: Program -> (Names, Names)
getExistencialAndUniversalVars program = (onlyInFirst exist_variables universal_variables, universal_variables)
  where
    get_variables :: Program -> (Names, Names)
    get_variables Skip                = ([], [])
    get_variables Empty               = ([], [])
    get_variables (Set x arit)        = ([], x:freeVars arit)
    get_variables (PSet x _)          = ([], [x])
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


programToSolverInput :: Program -> SolverInput'
programToSolverInput program = SolverInput' { solver_formulaes = concatMap restrictionsToImplications rest
                                             , existential = exist_vars
                                             , for_all = universal_vars }
  where
    (exist_vars, universal_vars) = getExistencialAndUniversalVars program
    (runt, rest) = vcGenerator0 program