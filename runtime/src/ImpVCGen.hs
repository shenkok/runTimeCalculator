module ImpVCGen where

import Imp

{-
    MODULO QUE SE ENCARGA DE GENERAR EL IMPUT PARA SBV A PARTIR
    DE LOS LENGUAJES IMPERATIVOS 
-}
-----------------------------------{RESTRICCIONES }-----------------------------------------------------

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
    p_true = p pe_b
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
vcGenerator (PWhile pe_b c inv) runt = (inv, (l_inv :!<=: inv) : snd vc_p)
  where
    p_true = p pe_b
    p_false = 1 - p_true
    vc_p = vcGenerator c inv
    l_inv = rtOne :++: ((p_false :**: runt) :++: (p_true :**: fst vc_p))


-- | Genera las restricciones considerando al 0 como runtime
vcGenerator0 :: Program -> (RunTime, [RRunTime])
vcGenerator0 program = vcGenerator program rtZero

----------------------------------{ OMEGA - CPO TÓPICOS}-------------------------------------------------

bottom :: RunTime
bottom = rtLit 0

top :: RunTime
top = rtLit (1/0)

-- | Función característica de un while
cfWhile :: BExp -> Program -> RunTime -> RunTime -> RunTime  
cfWhile b program runt x = rtOne :++: (((Not b) :<>: runt) :++: (b :<>: (fst (vcGenerator program x))))

-- | Función característica de un pwhile
cfPWhile :: PBExp -> Program -> RunTime -> RunTime ->  RunTime 
cfPWhile (Ber p) program runt x = rtOne :++: (((1 - p) :**: runt) :++: (p :**: (fst (vcGenerator program x))))

-- | Iteración de punto fijo para un while
fpWhile ::  RunTime -> BExp -> Program -> RunTime -> Integer -> RunTime
fpWhile x b program runt 0 = x
fpWhile x b program runt n = fpWhile (cfw x) b program runt (n - 1) where
  cfw = cfWhile b program runt

-- | Iteración de punto fijo para un pwhile
fpPWhile ::  RunTime -> PBExp -> Program -> RunTime -> Integer -> RunTime
fpPWhile x ber program runt 0 = x
fpPWhile x ber program runt n = fpPWhile (cfpw x) ber program runt (n - 1) where
  cfpw = cfPWhile ber program runt

-----------------------------------------------{PREPARACIÓN PARA SBV} -----------------------------------------

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
runTimeToArit  otherwise         = error $ "No hay versión directa a AExp" ++ show otherwise

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

