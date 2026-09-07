module ImpSynth where

import Imp
import ImpVCGen (freeVarsProgram)
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
