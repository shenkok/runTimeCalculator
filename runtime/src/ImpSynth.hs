module ImpSynth where

import Imp
import ImpVCGen
import ImpSBV (runModel')
import Data.SBV (AlgReal, SatResult, getModelValue, modelExists)
import qualified Data.Set as Set

{-
    MÓDULO DE SÍNTESIS: DESCUBRIR LA *FORMA* DE UN INVARIANTE

    Este módulo no verifica invariantes (eso es vcg[·] + ImpSBV) — se ocupa
    del paso anterior: dado un tiempo de ejecución "f" (típicamente un
    desenrollado de Kleene lo bastante profundo, ver fpWhile/fpPWhile en
    ImpVCGen), decidir de qué grado es el polinomio que lo describe, para
    poder armar un template con esa cantidad de coeficientes.

    El método es el de diferencias finitas, pero en vez de muestrear en unos
    pocos puntos concretos y confiar en que alcanzan, la comprobación se hace
    con una consulta ∃∀ a Z3:

        ∃c ∀x.  Δᵈf(x) = c

    Si Z3 la satisface, la conclusión es una identidad algebraica válida para
    TODO x — no evidencia sobre los puntos que uno haya elegido. Y "la
    d-ésima diferencia es una constante" equivale (por inducción sobre x) a
    que f sea un polinomio de grado d en x, así que el d más chico que
    satisface la consulta ES el grado del polinomio.

    La consulta se arma reusando la maquinaria que ya existe: la igualdad se
    parte en dos desigualdades (Restriction sólo tiene :<==:), y de ahí en
    más son restricciones de RunTime como cualquier otra, con la misma
    expansión de contextos de restrictionsToImplications.
-}

------------------------------{ DIFERENCIAS FINITAS SIMBÓLICAS }----------------------------------------

-- | f[x -> x+1]: corre el tiempo de ejecución un paso en la variable x.
shiftRunTime :: Name -> RunTime -> RunTime
shiftRunTime x = sustRunTime x (Var x +: Lit 1)

-- | Primera diferencia finita: Δf(x) = f(x+1) - f(x).
--
-- Es el análogo discreto de la derivada: para un polinomio de grado d, la
-- d-ésima diferencia es la constante d! por el coeficiente principal, igual
-- que la d-ésima derivada.
finiteDifference :: Name -> RunTime -> RunTime
finiteDifference x f = deepSimplifyRunTime (shiftRunTime x f --: f)

-- | Δᵈf: aplica la diferencia finita d veces (d = 0 devuelve f sin tocar).
nthDifference :: Int -> Name -> RunTime -> RunTime
nthDifference d x f = iterate (finiteDifference x) f !! max 0 d

------------------------------{ ARMADO DEL PROBLEMA ∃∀ }--------------------------------------------------

-- | Un nombre que no colisione con ninguno de los usados. Le va agregando
-- comillas simples hasta encontrar uno libre.
freshName :: Name -> Names -> Name
freshName base used
  | base `elem` used = freshName (base ++ "'") used
  | otherwise        = base

-- | "Δᵈf es exactamente la constante c", como par de restricciones de
-- RunTime: Restriction sólo tiene :<==:, así que la igualdad se escribe con
-- las dos desigualdades.
constantDifferenceRestrictions :: Name -> RunTime -> [RRunTime]
constantDifferenceRestrictions c diff = [ diff :<==: rtVar c, rtVar c :<==: diff ]

-- | Agrega hipótesis extra a todas las implicaciones.
--
-- Sirve para certificar la forma DENTRO de una pieza de la partición (ej.
-- sólo donde vale la guarda del ciclo). Es necesario porque el método es
-- local, igual que Taylor: un tiempo de ejecución piecewise no tiene una
-- diferencia constante si se lo mira cruzando el borde entre dos piezas,
-- aunque cada pieza por separado sí sea un polinomio limpio.
withHypotheses :: Context -> [Implication] -> [Implication]
withHypotheses extra = map addHyp
  where addHyp implication = implication { hypothesis = extra ++ hypothesis implication }

-- | El problema ∃c ∀x que certifica que la d-ésima diferencia de f es
-- constante en toda la región descrita por "piece".
--
-- "c" es existencial (es el valor que hay que encontrar) y todas las
-- variables que aparecen en la diferencia y en las hipótesis son
-- universales, así que el resultado vale para todos los estados de la
-- región, no sólo para los que uno hubiera muestreado.
constantDifferenceInput :: Name -> Context -> Int -> Name -> RunTime -> SolverInput'
constantDifferenceInput c piece d x f = SolverInput'
  { solver_formulaes = withHypotheses piece implications
  , existential      = Set.singleton c
  , for_all          = Set.fromList universales
  }
  where
    diff         = nthDifference d x f
    implications = concatMap restrictionsToImplications (constantDifferenceRestrictions c diff)
    universales  = filter (/= c) (rmdups (freeVarsRunTime diff ++ concatMap freeVarsBExp piece))

------------------------------{ BÚSQUEDA DEL GRADO }-----------------------------------------------------

-- | Resultado de certificar la forma de un tiempo de ejecución.
data DegreeCertificate = DegreeCertificate
  { certifiedDegree   :: Int      -- ^ grado del polinomio en la variable pedida
  , certifiedConstant :: AlgReal  -- ^ el valor de Δᵈf, constante en toda la región
  } deriving (Eq, Show)

-- | Certifica que Δᵈf es constante, para un d puntual.
certifyDegreeAt :: Context -> Int -> Name -> RunTime -> IO (Maybe DegreeCertificate)
certifyDegreeAt piece d x f = do
  let c  = freshName "c" (freeVarsRunTime f ++ concatMap freeVarsBExp piece)
      si = constantDifferenceInput c piece d x f
  result <- runModel' si
  return (if modelExists result then DegreeCertificate d <$> valueOf c result else Nothing)

-- | Busca el grado MÁS CHICO (hasta "cap") cuya diferencia finita es
-- constante en toda la región. Ese es el grado del polinomio que describe a
-- f ahí adentro, y por lo tanto la cantidad de coeficientes que necesita el
-- template.
--
-- Nothing significa "no es un polinomio de grado ≤ cap en esa región" — el
-- caso típico es una cola geométrica (las diferencias se achican a la mitad
-- en cada nivel pero nunca se hacen constantes), que corresponde a proponer
-- una constante existencial en vez de un polinomio.
certifyDegree :: Int -> Context -> Name -> RunTime -> IO (Maybe DegreeCertificate)
certifyDegree cap piece x f = go 0
  where
    go d
      | d > cap   = return Nothing
      | otherwise = do
          certificate <- certifyDegreeAt piece d x f
          case certificate of
            Just _  -> return certificate
            Nothing -> go (d + 1)

-- | Extrae el valor de una variable de un modelo de SBV.
valueOf :: Name -> SatResult -> Maybe AlgReal
valueOf = getModelValue
