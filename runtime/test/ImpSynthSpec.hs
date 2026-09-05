module ImpSynthSpec (spec) where

import Test.Hspec
import Data.Either (fromRight)
import qualified Data.Set as Set
import Imp hiding (it)
import ImpParser (parseBExp, parseProgram, parseRunTime)
import ImpVCGen
import ImpSynth

{-
  Tests de ImpSynth.hs: el paso de descubrir la FORMA de un invariante
  (su grado) antes de armar el template.

  Como ImpSBVSpec, estos tests invocan a Z3 de verdad: certifyDegree resuelve
  un problema ∃∀ por cada grado que prueba, y no hay forma de verificar qué
  certificó sin resolverlo. Las fórmulas se mantienen chicas (una variable,
  grado ≤ 2) para que sigan siendo rápidos.
-}

-- 1 + 2x, el tiempo de ejecución exacto de while(x>0){x:=x-1} sobre enteros
lineal :: RunTime
lineal = RunTimeArit (Lit 1 :+: (Lit 2 :*: Var "x"))

cuadratico :: RunTime
cuadratico = RunTimeArit (Var "x" :*: Var "x")

-- Desenrollado real de Kleene para while(x>0){x:=x-1}, continuación 0.
iteradoKleene :: Int -> RunTime
iteradoKleene k = deepSimplifyRunTime (fpWhile bottom guarda cuerpo cero k)
  where
    guarda = fromRight (error "guarda") (parseBExp "<test>" "x>0")
    cuerpo = fromRight (error "cuerpo") (parseProgram "<test>" "x:=x-1")
    cero   = fromRight (error "cero")   (parseRunTime "<test>" "0")

spec :: Spec
spec = do

  describe "diferencias finitas simbólicas" $ do

    it "shiftRunTime corre la variable un paso" $
      deepSimplifyRunTime (shiftRunTime "x" lineal)
        `shouldBe` deepSimplifyRunTime (RunTimeArit (Lit 3 :+: (Lit 2 :*: Var "x")))

    it "la primera diferencia de una recta es su pendiente" $
      finiteDifference "x" lineal `shouldBe` rtLit 2

    -- Δ(x²) = (x+1)² - x² = 2x + 1, y Δ²(x²) = 2 = 2! por el coeficiente
    -- principal — el análogo discreto de la segunda derivada.
    it "la segunda diferencia de una cuadrática es 2! por el coeficiente principal" $ do
      nthDifference 1 "x" cuadratico `shouldBe` RunTimeArit (Lit 1 :+: (Lit 2 :*: Var "x"))
      nthDifference 2 "x" cuadratico `shouldBe` rtLit 2

    it "la diferencia de orden 0 deja el RunTime intacto" $
      nthDifference 0 "x" lineal `shouldBe` lineal

  describe "freshName" $ do

    it "devuelve el nombre pedido si está libre" $
      freshName "c" ["x", "y"] `shouldBe` "c"

    it "evita colisionar con un nombre ya usado" $
      freshName "c" ["c", "x"] `shouldBe` "c'"

  describe "constantDifferenceInput" $ do

    it "deja la constante como única existencial y la variable como universal" $ do
      let si = constantDifferenceInput "c" [] 1 "x" cuadratico
      existential si `shouldBe` Set.singleton "c"
      for_all si `shouldBe` Set.fromList ["x"]

    it "las hipótesis de la pieza se agregan a todas las implicaciones" $ do
      let piece = [Var "x" >: Lit 0]
          si    = constantDifferenceInput "c" piece 1 "x" cuadratico
      all (\i -> head (hypothesis i) == Var "x" >: Lit 0) (solver_formulaes si)
        `shouldBe` True

  describe "certifyDegree" $ do

    it "una constante certifica grado 0, con su propio valor" $ do
      certificado <- certifyDegree 3 [] "x" (rtLit 3)
      certificado `shouldBe` Just (DegreeCertificate 0 3)

    it "una recta certifica grado 1, con su pendiente" $ do
      certificado <- certifyDegree 3 [] "x" lineal
      certificado `shouldBe` Just (DegreeCertificate 1 2)

    it "una cuadrática certifica grado 2" $ do
      certificado <- certifyDegree 3 [] "x" cuadratico
      certificado `shouldBe` Just (DegreeCertificate 2 2)

    it "si el tope de grado es menor al real, no certifica nada" $ do
      certificado <- certifyDegree 0 [] "x" lineal
      certificado `shouldBe` Nothing

  describe "certifyDegree sobre un desenrollado de Kleene real" $ do

    -- Regla de uso que estos dos tests fijan: un iterado de Kleene sólo es
    -- exacto hasta donde alcanzó su profundidad (hace falta k >= x+1 para
    -- resolver ese x). Más allá del frente confiable la función se aplana por
    -- truncamiento, y esa meseta rompe la diferencia constante — no porque el
    -- invariante no sea lineal, sino porque el iterado todavía no lo es.
    it "sin acotar la región, la meseta del truncamiento impide certificar" $ do
      certificado <- certifyDegree 2 [Var "x" >: Lit 0] "x" (iteradoKleene 9)
      certificado `shouldBe` Nothing

    it "acotado al frente confiable, certifica grado 1 con diferencia 2" $ do
      certificado <- certifyDegree 2 [Var "x" >: Lit 0, Var "x" :<=: Lit 6] "x" (iteradoKleene 9)
      certificado `shouldBe` Just (DegreeCertificate 1 2)

    -- La pieza [¬φ] del template natural: constante, y su valor es el tick de
    -- evaluar la guarda (cfWhile suma rtOne aunque la guarda dé falso).
    it "la pieza donde el ciclo no corre certifica grado 0 con valor 1" $ do
      certificado <- certifyDegree 2 [Var "x" :<=: Lit 0] "x" (iteradoKleene 9)
      certificado `shouldBe` Just (DegreeCertificate 0 1)
