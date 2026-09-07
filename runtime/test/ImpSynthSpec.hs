module ImpSynthSpec (spec) where

import Test.Hspec
import Data.Either (fromRight)
import Data.Maybe (isJust)
import Data.SBV (modelExists)
import Imp hiding (it)
import ImpParser (parseProgram, parseRunTime)
import ImpVCGen
import ImpSBV (runModel')
import ImpSynth

{-
  Tests de ImpSynth.hs: la síntesis de templates naturales, o sea proponer un
  invariante-plantilla para cada while/pwhile escrito sin invariante, a partir
  de la forma del programa. Diseño completo en SINTESIS_TEMPLATE_NATURAL.md.

  Los del último describe invocan a Z3 de verdad: verifican el mismo veredicto
  que reporta completeRoutine' (¿existe una instancia del template que sea
  invariante admisible?). Se mantienen chicos para que sigan siendo rápidos.
-}

spec :: Spec
spec = do

  describe "fillTemplates / synthesizeTemplates (templates naturales)" $ do

    it "rellena el hueco de un while sin invariante" $
      invariantsOf (synthesizeTemplates0 (getProgram "while(x > 0){x := x-1}"))
        `shouldSatisfy` all isJust

    it "un programa sin ciclos queda igual" $ do
      let prog = getProgram "x := 10; y := 3"
      synthesizeTemplates0 prog `shouldBe` prog

    it "respeta un invariante que el usuario ya escribió a mano" $
      invariantsOf (synthesizeTemplates0 (getProgram "while(x > 0){inv = 1 ++ 2**[x>0]**x}{x := x-1}"))
        `shouldBe` [Just (fromRight (error "no parsea") (parseRunTime "<test>" "1 ++ 2**[x>0]**x"))]

    it "rellena los dos huecos de un par de ciclos anidados" $
      invariantsOf (synthesizeTemplates0 (getProgram "while(x > 0){while(y > 0){y := y-1}; x := x-1}"))
        `shouldSatisfy` (\invs -> length invs == 2 && all isJust invs)

    -- Regresión: la guarda "x > 0" ya ES "Not (x <= 0)" (azúcar sintáctica), así
    -- que negarla sin simplificar deja "Not (Not (x <= 0))", y esa doble
    -- negación revienta después al linealizar ("No hay versión directa a AExp",
    -- ImpVCGen.runTimeToArit). La pieza [¬φ] tiene que salir ya simplificada.
    it "la pieza ¬φ no queda con doble negación cuando la guarda es azúcar" $
      map (fmap getBExp) (invariantsOf (synthesizeTemplates0 (getProgram "while(x > 0){x := x-1}")))
        `shouldBe` [Just [Var "x" :<=: Lit 0]]

    it "los coeficientes no chocan con variables del programa que se llamen igual" $ do
      -- El programa ya usa "a0", así que el primer coeficiente tiene que
      -- desviarse a otro nombre (freshName le agrega comillas).
      let prog = synthesizeTemplates0 (getProgram "while(a0 > 0){a0 := a0-1}")
          (exist, univ) = getExistencialAndUniversalVars prog
      exist `shouldNotContain` ["a0"]
      univ `shouldContain` ["a0"]

  describe "synthesizeTemplates de punta a punta (invoca Z3)" $ do

    it "while(x>0){x:=x-1}: encuentra testigo para el template afín" $
      isValidSynth "while(x > 0){x := x-1}" `shouldReturn` True

    it "while(c==1){c:~coin}: encuentra testigo (misma familia que p4_6/p4_8/p4_9)" $
      isValidSynth "while(c == 1){c :~ 1/2* <0> + 1/2* <1>}" `shouldReturn` True

    it "pwhile con un while anidado: resuelve los dos niveles juntos" $
      isValidSynth "pwhile(<9/10>){while(c == 1){c :~ 1/2* <0> + 1/2* <1>}}" `shouldReturn` True

    -- El costo de este programa es CUADRÁTICO en x (el ciclo interno corre x
    -- veces, x veces), y el template natural es afín: no existe instancia que
    -- sirva. Es el modo de fallo esperado — se reporta, no se refina.
    it "un ciclo de costo cuadrático no admite el template afín" $
      isValidSynth "while(x > 0){y := x; while(y > 0){y := y-1}; x := x-1}" `shouldReturn` False

-- | Invariantes de cada while/pwhile del programa, en orden, sin fallar ante
-- un hueco sin llenar (programInvariants sí falla: usa requireInvariant).
invariantsOf :: Program -> [Maybe RunTime]
invariantsOf (Seq p_1 p_2)        = invariantsOf p_1 ++ invariantsOf p_2
invariantsOf (If _ e_t e_f)       = invariantsOf e_t ++ invariantsOf e_f
invariantsOf (PIf _ e_t e_f)      = invariantsOf e_t ++ invariantsOf e_f
invariantsOf (While _ body minv)  = minv : invariantsOf body
invariantsOf (PWhile _ body minv) = minv : invariantsOf body
invariantsOf _                    = []

getProgram :: String -> Program
getProgram src = deepSimplifyProgram (fromRight (error ("no parsea: " ++ src)) (parseProgram "<test>" src))

-- | Sintetiza los templates del programa y pregunta a Z3 si existe una
-- instancia válida. Usa programToSolverInput (el singular, que junta todas las
-- obligaciones en un solo sistema) porque un programa sintetizado siempre
-- comparte variables de template entre sus obligaciones — resolverlas por
-- separado podría dar testigos contradictorios (mismo criterio que
-- ImpIO.completeRoutine' vía sharedExistentials).
isValidSynth :: String -> IO Bool
isValidSynth src = do
  result <- runModel' (programToSolverInput (synthesizeTemplates0 (getProgram src)))
  return (modelExists result)
