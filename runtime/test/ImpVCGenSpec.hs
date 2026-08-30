module ImpVCGenSpec (spec) where

import Test.Hspec

import ImpVCGen hiding (runtime)
import Imp hiding (it)
import ImpParser
import Data.Either (fromRight)

-- TODO: agrega los imports que falten (BExp, AExp, parser, etc.)

-- Helper para verificar que parsea correctamente
getRunTime :: String -> RunTime
getRunTime str = fromRight (error "parse error") (parseRunTime "" str)

spec :: Spec
spec = describe "restrictionsToImplications" $ do

  context "while(x > 0){inv = a ++ 2**[x>0]<>(x + c )}{x:= x-1}" $ do
    it "genera una única implicación con hipótesis vacía o trivial" $ do
      pending -- TODO: falta construir la Restriction RunTime (invariante :<==: funcionCaracteristica)
              -- y el [Implication] esperado; el `runtime` que se usaba acá no estaba definido.

  context "caso con una condición (2 contextos: p y ¬p)" $ do
    it "genera 2 implicaciones, una por cada valor de verdad de la condición" $ do
      pending -- TODO: definir la Restriction RunTime y el [Implication] esperado

  context "caso con dos condiciones anidadas (4 contextos)" $ do
    it "genera 4 implicaciones, replicando la combinatoria p∧q, ¬p∧q, p∧¬q, ¬p∧¬q" $ do
      pending -- TODO: definir la Restriction RunTime y el [Implication] esperado

  context "caso con restricción que requiere simplificación previa (paso 1)" $ do
    it "simplifica correctamente antes de extraer contextos" $ do
      pending -- TODO: caso tipo a:!<=:b -> a-b:!<=:0

  context "caso con variables existenciales y universales mezcladas" $ do
    it "no filtra por tipo de variable (ese manejo queda pendiente en la función)" $ do
      pending