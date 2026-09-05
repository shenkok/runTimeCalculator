module ImpProgramSpec (spec) where

import Test.Hspec
import Data.SBV (modelExists)
import Data.Either (fromRight)
import qualified Data.Set as Set
import Imp hiding (it)
import ImpParser (parseProgram)
import ImpVCGen
import ImpSBV (runModel')
import qualified ImpProgram as P

{-
  Corre, con el modo NUEVO de resolución (programToSolverInputs/mkUniversales,
  ∃∀ real — ver ImpVCGen.hs/ImpSBV.hs/ImpIO.hs), los programas del banco de la
  memoria (ImpProgram.hs) que corresponden a las categorías con INVARIANTE
  CONCRETO — sin variables de template/existenciales — de las 12
  subcategorías de validación del informe (Cap. 5 "Validación" y Anexo C).

  Se excluyen a propósito (no cubiertos acá, "sin usar templates"):
    - p4_7 (usa "A" como invariante-plantilla explícito).
    - Los programas p5_* (son sólo distribuciones PAExp sueltas, no Program).
    - Las expresiones/runtimes parseadas sueltas (arit_*/rtarit_*/ind_*), que
      tampoco son programas.

  A diferencia de la ImpVCGenSpec/ImpSBVSpec (unitarias, deterministas), acá
  SÍ se invoca a Z3 de verdad —una vez por cada while/pwhile de cada
  programa— porque estamos verificando el mismo tipo de veredicto
  ("válido"/"no válido") que reporta completeRoutine'.

  Varios de los veredictos esperados se confirmaron citando el cálculo MANUAL
  del propio informe (no sólo corriendo la herramienta y copiando el
  resultado):
    - Cdks  (cdks)      -> informe pp. 42-43 (Fig. 5.2/5.3, Listing 5.1)
    - Cdkc- (cdkcMenos)  -> informe pp. 44-46 (Fig. 5.4/5.5, Listing 5.2):
      contraejemplo x = 1/2, exactamente lo que reproduce el modo nuevo.
    - Cdkc+ (cdkcMas)    -> informe p. 68 (Fig. C.3/C.4, Anexo C.1.2).
    - Cdvs  (cdvs)       -> informe pp. 66-67 (Fig. C.1/C.2, Anexo C.1.1).
    - Cdvc+ (cdvcMas)    -> informe pp. 71-72 (Anexo C.1.3).
    - Cdvc- (cdvcMenos)  -> informe pp. 73-74 (Fig. C.7/C.8, Anexo C.1.4):
      contraejemplos x = -1 y x = 0. Ver el describe de más abajo dedicado a
      este caso — el modo nuevo no reproduce este veredicto.
    - Cpvs  (cpvs)       -> informe pp. 76-77 (Anexo C.1.5/C.1.6... Fig. C.11).
    - Cpkc- (cpkcMenos)  -> informe pp. 78-79 (Fig. C.13/C.14, Anexo C.1.7):
      obligaciones 9/2<=3 (inválida) ×2, 3<=3 (válida) — exactamente lo que
      reproduce el modo nuevo.
    - Cpvc  (cpvcMenos/cpvcMas) -> informe pp. 80-82 (Anexo C.1.8): I2^min
      (cpvcMenos) e I2 con otra aproximación del denominador (cpvcMas).
-}

-- | Parsea y simplifica un programa (mismo camino que "run"/"run'" en Main.hs).
getProgram :: String -> Program
getProgram src = deepSimplifyProgram (fromRight (error ("no parsea: " ++ src)) (parseProgram "<test>" src))

-- | Resuelve TODAS las obligaciones de un programa con el modo nuevo y da el
-- veredicto global: True si todas las obligaciones son válidas. Maneja las
-- dos polaridades de "válida" según haya o no variables existenciales (ver
-- showSolverInput' en ImpIO.hs, mismo criterio).
isValid :: Program -> IO Bool
isValid program = do
  results <- mapM solveOne (programToSolverInputs program)
  return (and results)
  where
    solveOne si = do
      r <- runModel' si
      return $ if Set.null (existential si) then not (modelExists r) else modelExists r

spec :: Spec
spec = describe "ImpProgram (banco de la memoria), modo nuevo, sólo invariantes concretos" $ do

  describe "programas deterministicos sin ciclo (siempre válidos, sin obligaciones)" $ do
    it "Cdks (cdks)" $
      isValid (getProgram P.cdks) `shouldReturn` True
    it "Cdvs (cdvs)" $
      isValid (getProgram P.cdvs) `shouldReturn` True
    it "p1_1, p1_2, p1_3, p1_4" $ do
      isValid (getProgram P.p1_1) `shouldReturn` True
      isValid (getProgram P.p1_2) `shouldReturn` True
      isValid (getProgram P.p1_3) `shouldReturn` True
      isValid (getProgram P.p1_4) `shouldReturn` True

  describe "programas deterministicos con ciclo" $ do
    it "Cdkc- (cdkcMenos): invariante '1 + 2*[x>0]*x' insuficiente para x racional -> no válido" $
      isValid (getProgram P.cdkcMenos) `shouldReturn` False
    it "Cdkc+ (cdkcMas): mismo invariante con holgura '+1' -> válido" $
      isValid (getProgram P.cdkcMas) `shouldReturn` True
    it "p2_1: mismo invariante insuficiente que Cdkc- (sin el x:=3 previo) -> no válido" $
      isValid (getProgram P.p2_1) `shouldReturn` False
    it "p2_2: invariante con guarda 'y>=10' -> válido" $
      isValid (getProgram P.p2_2) `shouldReturn` True
    it "Cdvc+ (cdvcMas): invariante con guarda compuesta 'y<=x && x<=z' -> válido" $
      isValid (getProgram P.cdvcMas) `shouldReturn` True

  describe "programas probabilisticos sin ciclo (siempre válidos, sin obligaciones)" $ do
    it "Cpks (cpks)" $
      isValid (getProgram P.cpks) `shouldReturn` True
    it "Cpvs (cpvs)" $
      isValid (getProgram P.cpvs) `shouldReturn` True
    it "cTrunc" $
      isValid (getProgram P.cTrunc) `shouldReturn` True

  describe "programas probabilisticos con ciclo" $ do
    it "p4_1: un único pwhile trivial -> válido" $
      isValid (getProgram P.p4_1) `shouldReturn` True
    it "Cpkc+ (cpkcMas): tres pwhile en secuencia con invariantes fijos exactos (9,6,3) -> todos válidos" $
      isValid (getProgram P.cpkcMas) `shouldReturn` True
    it "Cpkc- (cpkcMenos): for(3){pwhile(pinv=3)} -> 2 obligaciones inválidas, 1 válida => no válido en conjunto" $
      isValid (getProgram P.cpkcMenos) `shouldReturn` False
    it "p4_2: while(c==1){inv=1+4*[c==1]} -> válido" $
      isValid (getProgram P.p4_2) `shouldReturn` True
    it "p4_6: mismo caso que p4_2, K=4 es la cota exacta -> válido" $
      isValid (getProgram P.p4_6) `shouldReturn` True
    it "p4_8: K=3 subestima la cota exacta (4) -> no válido" $
      isValid (getProgram P.p4_8) `shouldReturn` False
    it "p4_9: K=5 sobreestima la cota exacta pero sigue siendo válida (holgada)" $
      isValid (getProgram P.p4_9) `shouldReturn` True
    it "p4_3: pwhile anidando un while -> alguna obligación no válida" $
      isValid (getProgram P.p4_3) `shouldReturn` False
    it "cpvcMas: aproximación del invariante que deja ambas obligaciones válidas" $
      isValid (getProgram P.cpvcMas) `shouldReturn` True
    it "cpvcMenos: otra aproximación (I2^min del informe) que deja el while interno no válido" $
      isValid (getProgram P.cpvcMenos) `shouldReturn` False
    it "cpvc: variante base, sin sufijo Mas/Menos -> alguna obligación no válida" $
      isValid (getProgram P.cpvc) `shouldReturn` False

  describe "precision numerica (invariantes con constantes racionales muy cercanas al limite)" $ do
    it "p4_10, p4_14: holgura entera '+1'/'+2' -> válidos" $ do
      isValid (getProgram P.p4_10) `shouldReturn` True
      isValid (getProgram P.p4_14) `shouldReturn` True
    it "p4_11, p4_13, p4_15: holgura fraccionaria mínima (ej. 2439/2438) -> siguen siendo válidos" $ do
      isValid (getProgram P.p4_11) `shouldReturn` True
      isValid (getProgram P.p4_13) `shouldReturn` True
      isValid (getProgram P.p4_15) `shouldReturn` True

  describe "caso donde la heurística de variables existenciales confunde al modo nuevo (documentado, no arreglado acá)" $ do
    it "Cdvc- (cdvcMenos): el informe prueba a mano que NO es válido (contraejemplos x=-1 y x=0, pp. 73-74); \
       \el modo nuevo clasifica 'x' como EXISTENCIAL -no aparece asignada ni en ninguna guarda real, la \
       \guarda del while es la constante false- y por eso responde 'válido' con testigo x=1. No es un bug \
       \del solver ni de mkUniversales: es la misma heurística puramente sintáctica de \
       \getExistencialAndUniversalVars ya documentada en CLAUDE.md, que acá se equivoca de rol para 'x' \
       \porque este programa de prueba nunca la usa en ningún otro lado." $ do
      let program = getProgram P.cdvcMenos
          sis      = programToSolverInputs program
      -- Confirma el diagnóstico: "x" quedó existencial, no universal.
      map (Set.toList . existential) sis `shouldBe` [["x"]]
      map (Set.toList . for_all) sis `shouldBe` [[]]
      -- Y por eso el modo nuevo da "válido" acá, al revés de lo que el
      -- informe demuestra a mano para el mismo programa.
      isValid program `shouldReturn` True
