module ImpSynth where

import Imp
import ImpVCGen (freeVarsProgram, programInvariants)
import Control.Monad.Trans.State (State, evalState, get, put)

{-
    MÓDULO DE SÍNTESIS: TEMPLATES NATURALES

    Rellena los huecos de invariante de un programa (los While/PWhile con
    Nothing) con templates propuestos a partir de la *forma* del programa, para
    que después vcGenerator'/programToSolverInput pidan a Z3, en una única
    consulta ∃∀, los valores de sus coeficientes. Sin lazo de refinamiento: un
    intento, un veredicto.

    La regla (de los "natural templates" de Batz et al., TACAS 2023) es que el
    invariante de un ciclo se arma con dos piezas:

      - la pieza [¬φ] (o el peso 1-p en un pwhile) multiplica LA CONTINUACIÓN,
        sin coeficientes propios: cuando la guarda es falsa el valor ya se
        conoce exactamente, es "f" por definición de Φ_f, y gastar incógnitas
        ahí sería redundante;
      - la pieza [φ] (o el peso p) multiplica una combinación AFÍN de las
        variables relevantes del ciclo, con coeficientes frescos de este nivel.

    El recorrido es el mismo de vcGenerator', y como toda recursión usa las dos
    direcciones del stack: la continuación baja como argumento (es lo que
    consume la pieza [¬φ]) y el ert sube como resultado (es lo que Seq necesita
    para darle continuación al statement de la izquierda). Como la pieza [φ]
    lleva coeficientes frescos y no el valor que sube desde el cuerpo, ningún
    template depende del ert de su propio cuerpo: no hay circularidad.

    Diseño completo, cuidados y validación previa: SINTESIS_TEMPLATE_NATURAL.md
    en la raíz del repo.
-}

-- | Fuente de nombres frescos. El estado es un contador (para numerar los
-- coeficientes a0, a1, a2...) junto a los nombres ya ocupados: variables del
-- programa y coeficientes ya asignados en otros niveles. Así ningún
-- coeficiente choca con nada y getExistencialAndUniversalVars los clasifica
-- como existenciales sin ambigüedad.
type Fresh = State (Int, Names)

-- | Un nombre que no colisione con ninguno de los usados. Le va agregando
-- comillas simples hasta encontrar uno libre.
freshName :: Name -> Names -> Name
freshName base used
  | base `elem` used = freshName (base ++ "'") used
  | otherwise        = base

-- | Reserva un nombre de coeficiente nuevo y lo marca como ocupado.
freshCoefficient :: Fresh Name
freshCoefficient = do
  (n, used) <- get
  let name = freshName ("a" ++ show n) used
  put (n + 1, name : used)
  return name

-- | Combinación afín con coeficientes frescos sobre las variables dadas:
-- a_0 ++ a_1**v_1 ++ ... ++ a_n**v_n (sólo a_0 si no hay variables).
--
-- Grado 1 y nada más, que es la forma de los natural templates del paper.
-- Subir el grado se probó y se descartó por ahora: ver "Experimento: subir el
-- template a grado 2" en SINTESIS_TEMPLATE_NATURAL.md — funciona en casos
-- chicos pero la consulta se vuelve intratable justo donde haría falta.
freshAffine :: Names -> Fresh RunTime
freshAffine vars = do
  a_0 <- freshCoefficient
  terms <- mapM weighted (rmdups vars)
  return (foldl (:++:) (rtVar a_0) terms)
  where
    weighted v = do
      a_i <- freshCoefficient
      return (rtVar a_i :**: rtVar v)

-- | Template natural de un while: [¬φ] pesa la continuación, [φ] pesa una
-- afín con coeficientes frescos.
--
-- La negación de la guarda PASA POR simplifyBExp a propósito: las guardas
-- azucaradas ya son negaciones (">" es "Not (<=)"), así que "Not e_b" crudo
-- deja una doble negación que revienta más adelante al linealizar
-- ("No hay versión directa a AExp", ImpVCGen.runTimeToArit).
naturalTemplateWhile :: BExp -> Program -> RunTime -> Fresh RunTime
naturalTemplateWhile e_b body f = do
  t_phi <- freshAffine (freeVarsBExp e_b ++ freeVarsProgram body)
  return $ (RunTimeBExp (simplifyBExp (Not e_b)) :**: (rtOne :++: f))
      :++: (RunTimeBExp e_b                      :**: (rtOne :++: t_phi))

-- | Template natural de un pwhile. Misma partición que el while, pero pesada
-- por las constantes (1-p)/p en vez de por indicatrices: un pwhile no tiene
-- guarda booleana, su "condición" es una moneda. Es exactamente la forma en
-- que vcGenerator' arma su l_inv.
naturalTemplatePWhile :: PBExp -> Program -> RunTime -> Fresh RunTime
naturalTemplatePWhile pe_b body f = do
  t_phi <- freshAffine (freeVarsProgram body)
  let p_true = p pe_b
  return $ (rtLit (1 - p_true) :**: (rtOne :++: f))
      :++: (rtLit p_true       :**: (rtOne :++: t_phi))

-- | Recorre el programa rellenando cada hueco de invariante.
--
-- f     : continuación de este programa (ya cerrada, viene bajando)
-- (p',t): el programa con los Nothing reemplazados por Just template, y el
--         ert que este programa devuelve hacia arriba.
--
-- Cada caso replica el ert que ya calcula vcGenerator'; lo único nuevo es el
-- relleno de huecos.
fillTemplates :: Program -> RunTime -> Fresh (Program, RunTime)
fillTemplates Skip       f = return (Skip,       rtOne :++: f)
fillTemplates Empty      f = return (Empty,      f)
fillTemplates (Set x arit)   f = return (Set x arit,   rtOne :++: sustRunTime x arit f)
fillTemplates (PSet x parit) f = return (PSet x parit, rtOne :++: aexpE parit x f)

-- Acá es donde hay que tocar fondo y volver: el ert de p_2 es la continuación
-- de p_1, así que p_2 se procesa primero (igual que en vcGenerator').
fillTemplates (Seq p_1 p_2) f = do
  (p_2', f_2) <- fillTemplates p_2 f
  (p_1', f_1) <- fillTemplates p_1 f_2
  return (Seq p_1' p_2', f_1)

fillTemplates (If e_b e_t e_f) f = do
  (e_t', f_t) <- fillTemplates e_t f
  (e_f', f_f) <- fillTemplates e_f f
  return ( If e_b e_t' e_f'
         , rtOne :++: ((RunTimeBExp e_b :**: f_t) :++: (RunTimeBExp (Not e_b) :**: f_f)) )

fillTemplates (PIf pe_b e_t e_f) f = do
  (e_t', f_t) <- fillTemplates e_t f
  (e_f', f_f) <- fillTemplates e_f f
  let p_true = p pe_b
  return ( PIf pe_b e_t' e_f'
         , rtOne :++: ((rtLit p_true :**: f_t) :++: (rtLit (1 - p_true) :**: f_f)) )

-- El cuerpo recibe como continuación el propio invariante, y lo que sube es
-- ese invariante (regla ert[while(φ){C}][I] = I), no el ert del cuerpo — por
-- eso el "_". Un invariante ya escrito por el usuario se respeta tal cual,
-- pero igual hay que seguir bajando: el cuerpo puede tener huecos.
fillTemplates (While e_b body minv) f = do
  inv <- maybe (naturalTemplateWhile e_b body f) return minv
  (body', _) <- fillTemplates body inv
  return (While e_b body' (Just inv), inv)

fillTemplates (PWhile pe_b body minv) f = do
  inv <- maybe (naturalTemplatePWhile pe_b body f) return minv
  (body', _) <- fillTemplates body inv
  return (PWhile pe_b body' (Just inv), inv)

-- | Nombres ya ocupados por el programa: sus variables más las que aparezcan
-- en los invariantes que el usuario ya haya escrito a mano.
--
-- No se puede usar programInvariants acá: esa función falla con requireInvariant
-- justamente ante un hueco sin llenar, que es el caso normal en esta etapa.
usedNames :: Program -> Names
usedNames program = rmdups (freeVarsProgram program ++ go program)
  where
    go (Seq p_1 p_2)        = go p_1 ++ go p_2
    go (If _ e_t e_f)       = go e_t ++ go e_f
    go (PIf _ e_t e_f)      = go e_t ++ go e_f
    go (While _ body minv)  = maybe [] freeVarsRunTime minv ++ go body
    go (PWhile _ body minv) = maybe [] freeVarsRunTime minv ++ go body
    go _                    = []

-- | Rellena todos los huecos de invariante de un programa, dada su
-- continuación. El resultado ya se puede pasar a vcGenerator'.
synthesizeTemplates :: Program -> RunTime -> Program
synthesizeTemplates program f =
  fst (evalState (fillTemplates program f) (0, rmdups (usedNames program ++ freeVarsRunTime f)))

-- | synthesizeTemplates con continuación 0, que es la que usan
-- vcGenerator0/completeRoutine'.
synthesizeTemplates0 :: Program -> Program
synthesizeTemplates0 program = synthesizeTemplates program rtZero

{-
    SUGERENCIAS POR ITERACIÓN DE KLEENE

    Antes de proponer el template, a cada ciclo sin invariante se le muestran
    sus primeros iterados de punto fijo Φ_f⁰(0), ..., Φ_fⁿ(0), como pista
    para que el usuario adivine el invariante a mano (técnica de la memoria,
    §5.4.3 / Anexo C.1.8). No se usan para nada más: no certifican ni
    alimentan al template.
-}

-- | Cantidad de iteraciones que se muestran (además del iterado 0).
kleeneDepth :: Int
kleeneDepth = 4

-- | Iterados de Kleene de un ciclo, ya pasados por el simplificador.
--
-- Pista para un ciclo sin invariante. iterates es Nothing cuando el ciclo
-- está anidado dentro de otro que tampoco tiene invariante: su continuación
-- depende de ese invariante desconocido, así que no tiene iterados propios.
data KleeneHint = KleeneHint
  { loopHeader :: String
  , iterates   :: Maybe [RunTime]
  }

-- | ert aproximado: igual que vcGenerator, salvo que un ciclo sin invariante
-- se reemplaza por su n-ésimo iterado de Kleene (en vez de fallar con
-- requireInvariant). Un ciclo con invariante devuelve el invariante, igual
-- que vcGenerator.
ertApprox :: Int -> Program -> RunTime -> RunTime
ertApprox _ Skip           f = rtOne :++: f
ertApprox _ Empty          f = f
ertApprox _ (Set x arit)   f = rtOne :++: sustRunTime x arit f
ertApprox _ (PSet x parit) f = rtOne :++: aexpE parit x f
ertApprox n (Seq p_1 p_2)  f = ertApprox n p_1 (ertApprox n p_2 f)
ertApprox n (If e_b e_t e_f) f =
  rtOne :++: ((RunTimeBExp e_b :**: ertApprox n e_t f) :++: (RunTimeBExp (Not e_b) :**: ertApprox n e_f f))
ertApprox n (PIf pe_b e_t e_f) f =
  rtOne :++: ((rtLit (p pe_b) :**: ertApprox n e_t f) :++: (rtLit (1 - p pe_b) :**: ertApprox n e_f f))
ertApprox _ (While _ _ (Just inv))  _ = inv
ertApprox n (While e_b body Nothing) f = last (whileIterates n e_b body f)
ertApprox _ (PWhile _ _ (Just inv)) _ = inv
ertApprox n (PWhile pe_b body Nothing) f = last (pwhileIterates n pe_b body f)

-- | Φ_f⁰(0), ..., Φ_fⁿ(0) de un while: la misma función característica que
-- cfWhile, pero con el cuerpo vía ertApprox para tolerar ciclos internos sin
-- invariante. Se simplifica en cada paso para que el tamaño no explote.
whileIterates :: Int -> BExp -> Program -> RunTime -> [RunTime]
whileIterates n e_b body f = take (n + 1) (iterate step rtZero)
  where
    step x = deepSimplifyRunTime $
      rtOne :++: ((RunTimeBExp (simplifyBExp (Not e_b)) :**: f) :++: (RunTimeBExp e_b :**: ertApprox n body x))

-- | Igual que whileIterates, para un pwhile (función característica de cfPWhile).
pwhileIterates :: Int -> PBExp -> Program -> RunTime -> [RunTime]
pwhileIterates n pe_b body f = take (n + 1) (iterate step rtZero)
  where
    p_true = p pe_b
    step x = deepSimplifyRunTime $
      rtOne :++: ((rtLit (1 - p_true) :**: f) :++: (rtLit p_true :**: ertApprox n body x))

-- | Una pista por cada ciclo sin invariante, en el mismo orden que
-- programInvariants (ciclo antes que su cuerpo, izquierda antes que derecha),
-- con continuación inicial 0 (la misma de vcGenerator0).
kleeneHints :: Int -> Program -> [KleeneHint]
kleeneHints n program = go program (Just rtZero)
  where
    -- La continuación es Nothing dentro de un ciclo sin invariante.
    go (Seq p_1 p_2) mf     = go p_1 (ertApprox n p_2 <$> mf) ++ go p_2 mf
    go (If _ e_t e_f) mf    = go e_t mf ++ go e_f mf
    go (PIf _ e_t e_f) mf   = go e_t mf ++ go e_f mf
    go (While _ body (Just inv))  _ = go body (Just inv)
    go (PWhile _ body (Just inv)) _ = go body (Just inv)
    go (While e_b body Nothing) mf =
      KleeneHint ("while(" ++ show e_b ++ ")") (whileIterates n e_b body <$> mf) : go body Nothing
    go (PWhile pe_b body Nothing) mf =
      KleeneHint ("pwhile(" ++ show pe_b ++ ")") (pwhileIterates n pe_b body <$> mf) : go body Nothing
    go _ _ = []

-- | Los templates que synthesizeTemplates0 le asigna a cada ciclo que venía
-- sin invariante, en el mismo orden que kleeneHints.
synthesizedTemplates :: Program -> [RunTime]
synthesizedTemplates program =
  [ inv | (Nothing, inv) <- zip (holes program) (programInvariants (synthesizeTemplates0 program)) ]
  where
    holes (Seq p_1 p_2)        = holes p_1 ++ holes p_2
    holes (If _ e_t e_f)       = holes e_t ++ holes e_f
    holes (PIf _ e_t e_f)      = holes e_t ++ holes e_f
    holes (While _ body minv)  = minv : holes body
    holes (PWhile _ body minv) = minv : holes body
    holes _                    = []
