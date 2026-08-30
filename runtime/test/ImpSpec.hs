module ImpSpec (spec) where

import Test.Hspec
import Imp hiding (it)

spec :: Spec
spec = describe "Imp" $ do

  describe "completeNormArit" $ do

    -- Casos básicos
    it "literal simple no cambia" $ do
      completeNormArit (Lit 3) `shouldBe` Lit 3

    it "variable simple no cambia" $ do
      completeNormArit (Var "x") `shouldBe` Var "x"

    it "elimina coeficiente cero" $ do
      completeNormArit (0 :*: Var "x") `shouldBe` Lit 0

    it "elimina coeficiente uno" $ do
      completeNormArit (1 :*: Var "x") `shouldBe` Var "x"

    it "agrupa coeficientes de la misma variable" $ do
      completeNormArit (Var "x" :+: Var "x") `shouldBe` (2 :*: Var "x")

    it "suma con cero desaparece" $ do
      completeNormArit (Var "x" :+: Lit 0) `shouldBe` Var "x"

    -- Casos más complejos
    it "resta de la misma variable da cero" $ do
      completeNormArit (Var "x" -: Var "x") `shouldBe` Lit 0

    it "agrupa múltiples instancias de la misma variable" $ do
      completeNormArit (Var "x" :+: Var "x" :+: Var "x") `shouldBe` (3 :*: Var "x")

    it "normaliza dos variables distintas" $ do
      completeNormArit (Var "x" :+: Var "y") `shouldBe` (Var "x" :+: Var "y")

    it "combina constante y variable" $ do
      completeNormArit (Lit 3 :+: (2 :*: Var "x")) `shouldBe` (Lit 3 :+: (2 :*: Var "x"))

    it "suma ponderada de dos variables" $ do
      completeNormArit ((2 :*: Var "x") :+: (3 :*: Var "x")) `shouldBe` (5 :*: Var "x")

    it "expresión con tres variables distintas" $ do
      completeNormArit (Var "x" :+: Var "y" :+: Var "z")
        `shouldBe` (Var "x" :+: Var "y" :+: Var "z")

    it "coeficientes mixtos con resta" $ do
      completeNormArit ((3 :*: Var "x") -: (1 :*: Var "x")) `shouldBe` (2 :*: Var "x")

    it "expresión compleja con constante y múltiples variables" $ do
      completeNormArit (Lit 5 :+: (2 :*: Var "x") :+: (3 :*: Var "y") :+: Var "x")
        `shouldBe` (Lit 5 :+: (3 :*: Var "x") :+: (3 :*: Var "y"))

    -- 3x + 56xy² - 18x*3y*y = 3x + 56xy² - 54xy² = 3x + 2xy²
    it "simplifica un término no lineal: 3x + 56xy² - 18x*3y*y = 3x + 2xy²" $ do
      let expr = ((3 :*: Var "x") :+: (56 :*: (Var "x" :*: (Var "y" :*: Var "y"))))
                   -: ((18 :*: Var "x") :*: ((3 :*: Var "y") :*: Var "y"))
      completeNormArit expr
        `shouldBe` ((3 :*: Var "x") :+: (2 :*: (Var "x" :*: (Var "y" :*: Var "y"))))

    -- 29xy² - 9x*y*3y - 3x + 6x = 29xy² - 27xy² + 3x = 3x + 2xy² (mismo resultado que el test anterior)
    it "simplifica otra forma equivalente: 29xy² - 9x*y*3y - 3x + 6x = 3x + 2xy²" $ do
      let term1 = 29 :*: (Var "x" :*: (Var "y" :*: Var "y"))
          term2 = (9 :*: Var "x") :*: (Var "y" :*: (3 :*: Var "y"))
          expr  = ((term1 -: term2) -: (3 :*: Var "x")) :+: (6 :*: Var "x")
      completeNormArit expr
        `shouldBe` ((3 :*: Var "x") :+: (2 :*: (Var "x" :*: (Var "y" :*: Var "y"))))

  describe "aexpE" $ do

    -- Distribución de Dirac: E[runt] con x=c es simplemente runt[x:=c]
    it "dirac sustituye directamente" $ do
      -- E_{x ~ dirac(2)}[x] = 2
      aexpE (dirac (Lit 2)) "x" (rtVar "x")
        `shouldBe` RunTimeArit (Lit 2)

    it "dirac con runtime constante no cambia" $ do
      -- E_{x ~ dirac(2)}[1] = 1
      aexpE (dirac (Lit 2)) "x" rtOne
        `shouldBe` rtOne

    it "dirac con variable distinta no sustituye" $ do
      -- E_{x ~ dirac(2)}[y] = y
      aexpE (dirac (Lit 2)) "x" (rtVar "y")
        `shouldBe` rtVar "y"

    -- Distribución coin: E[runt] = p*runt[x:=0] + (1-p)*runt[x:=1]
    it "coin calcula esperanza de variable" $ do
      -- E_{x ~ coin(0.5)}[x] = 0.5*0 + 0.5*1 = 0.5
      aexpE (coin 0.5) "x" (rtVar "x")
        `shouldBe` RunTimeArit (Lit 0.5)

    it "coin con runtime constante no cambia" $ do
      -- E_{x ~ coin(0.5)}[1] = 0.5*1 + 0.5*1 = 1
      aexpE (coin 0.5) "x" rtOne
        `shouldBe` rtOne

    it "coin con p=1 equivale a dirac(0)" $ do
      -- E_{x ~ coin(1)}[x] = 1*0 + 0*1 = 0
      aexpE (coin 1) "x" (rtVar "x")
        `shouldBe` rtZero

    it "coin con p=0 equivale a dirac(1)" $ do
      -- E_{x ~ coin(0)}[x] = 0*0 + 1*1 = 1
      aexpE (coin 0) "x" (rtVar "x")
        `shouldBe` rtOne

    -- Linealidad de la esperanza
    it "linealidad: E[x + y] con dirac" $ do
      -- E_{x ~ dirac(3)}[x + 1] = 4
      aexpE (dirac (Lit 3)) "x" 
            (RunTimeArit (Var "x") :++: RunTimeArit (Lit 1))
        `shouldBe` RunTimeArit (Lit 4)

    it "escala por constante: E[2*x] con dirac(3) = 6" $ do
      aexpE (dirac (Lit 3)) "x"
            (2 :**: rtVar "x")
        `shouldBe` RunTimeArit (Lit 6)

  describe "expectedValue" $ do

    it "esperanza de lista vacía es el base" $ do
      -- E_{vacío}[id] = 0
      expectedValue [] id (*) (+) (0 :: Constant)
        `shouldBe` 0

    it "distribución de masa 1 aplica transform directamente" $ do
      -- E_{[(1, 5)]}[id] = 1 * 5 = 5
      expectedValue [(1, 5 :: Constant)] id (*) (+) 0
        `shouldBe` 5

    it "suma ponderada de dos elementos" $ do
      -- E_{[(0.3, 10), (0.7, 20)]}[id] = 0.3*10 + 0.7*20 = 17
      expectedValue [(0.3, 10), (0.7, 20 :: Constant)] id (*) (+) 0
        `shouldBe` 17