module ImpParserSpec (spec) where

import Test.Hspec
import Text.Parsec (ParseError)
import Imp hiding (it)
import ImpParser

-- Función auxiliar para parsear Runtime 



-- Helper para verificar que falla
shouldFail :: (Show a) => Either ParseError a -> Expectation
shouldFail result = result `shouldSatisfy` isLeft
  where isLeft (Left _) = True
        isLeft _        = False
-- Helper para verificar que parsea correctamente
shouldParseTo :: (Show a, Eq a) => Either ParseError a -> a -> Expectation
shouldParseTo result expected = result `shouldBe` Right expected

spec :: Spec
spec = describe "ImpParser" $ do

  -- ----------------------------------------------------------------
  describe "parseAExp" $ do

    it "parsea un literal entero" $
      parseAExp "" "3"
        `shouldParseTo` Lit 3

    it "parsea un literal cero" $
      parseAExp "" "0"
        `shouldParseTo` Lit 0

    it "parsea una variable" $
      parseAExp "" "x"
        `shouldParseTo` Var "x"

    it "parsea una suma de variables" $
      parseAExp "" "x + y"
        `shouldParseTo` (Var "x" :+: Var "y")

    it "parsea una resta de variables" $
      parseAExp "" "x - y"
        `shouldParseTo` (Var "x" -: Var "y")

    it "parsea ponderación por constante" $
      parseAExp "" "2 * x"
        `shouldParseTo` (2 :*: Var "x")

    it "parsea un racional fraccionario" $
      parseAExp "" "1/2"
        `shouldParseTo` Lit (1/2)

    it "parsea expresión con paréntesis" $
      parseAExp "" "2 * (x + y)"
        `shouldParseTo` (2 :*: (Var "x" :+: Var "y"))

    it "parsea suma de literal y variable" $
      parseAExp "" "1 + x"
        `shouldParseTo` (Lit 1 :+: Var "x")

    it "falla con string vacío" $
      shouldFail (parseAExp "" "")

  -- ----------------------------------------------------------------
  describe "parseBExp" $ do

    it "parsea true" $
      parseBExp "" "true"
        `shouldParseTo` True'

    it "parsea false" $
      parseBExp "" "false"
        `shouldParseTo` False'

    it "parsea menor igual" $
      parseBExp "" "x <= y"
        `shouldParseTo` (Var "x" :<=: Var "y")

    it "parsea igualdad" $
      parseBExp "" "x == y"
        `shouldParseTo` (Var "x" :==: Var "y")

    it "parsea mayor igual" $
      parseBExp "" "x >= y"
        `shouldParseTo` (Var "x" >=: Var "y")

    it "parsea negación" $
      parseBExp "" "!true"
        `shouldParseTo` Not True'

    it "parsea and lógico" $
      parseBExp "" "true && false"
        `shouldParseTo` (True' :&: False')

    it "parsea or lógico" $
      parseBExp "" "true || false"
        `shouldParseTo` (True' :|: False')

    it "parsea expresión compuesta" $
      parseBExp "" "x <= y && y <= z"
        `shouldParseTo` ((Var "x" :<=: Var "y") :&: (Var "y" :<=: Var "z"))

    it "falla con string vacío" $
      shouldFail (parseBExp "" "")

  -- ----------------------------------------------------------------
  describe "parseRunTime" $ do

    it "parsea runtime literal" $
      parseRunTime "" "1"
        `shouldParseTo` RunTimeArit (Lit 1)

    it "parsea runtime variable" $
      parseRunTime "" "x"
        `shouldParseTo` RunTimeArit (Var "x")

    it "parsea indicatriz true" $
      parseRunTime "" "[true]"
        `shouldParseTo` toIndicator True'

    it "parsea indicatriz con condición" $
      parseRunTime "" "[x <= y]"
        `shouldParseTo` toIndicator (Var "x" :<=: Var "y")

    -- La vieja sintaxis dedicada "[b] <> algo" se retiró junto con el
    -- constructor :<>: — ahora una indicatriz ponderada por una expresión
    -- aritmética se escribe con "**", igual que cualquier otro producto de
    -- runtimes (ver "parsea indicatriz por runtime arbitrario" más abajo).
    it "parsea indicatriz por aritmética" $
      parseRunTime "" "[x <= y] ** x"
        `shouldParseTo` (RunTimeBExp (Var "x" :<=: Var "y") :**: RunTimeArit (Var "x"))

    -- A diferencia de la vieja sintaxis "<>" (restringida a indicatriz por
    -- aexp), "**" ya es un operador infijo genérico entre dos runtime
    -- cualesquiera: acá el segundo factor es él mismo un runtime compuesto
    -- (una suma), no un aexp simple.
    it "parsea indicatriz por runtime arbitrario" $
      parseRunTime "" "[x <= y] ** (x ++ y)"
        `shouldParseTo` (RunTimeBExp (Var "x" :<=: Var "y") :**: (RunTimeArit (Var "x") :++: RunTimeArit (Var "y")))

    it "parsea suma de runtimes" $
      parseRunTime "" "x ++ y"
        `shouldParseTo` (RunTimeArit (Var "x") :++: RunTimeArit (Var "y"))

    it "parsea resta de runtimes" $
      parseRunTime "" "x -- y"
        `shouldParseTo` (RunTimeArit (Var "x") --: RunTimeArit (Var "y"))

    it "parsea ponderación por constante" $
      parseRunTime "" "2 ** x"
        `shouldParseTo` (2 :**: RunTimeArit (Var "x"))

    it "parsea runtime con paréntesis" $
      parseRunTime "" "2 ** (x ++ y)"
        `shouldParseTo` (2 :**: (RunTimeArit (Var "x") :++: RunTimeArit (Var "y")))

    it "falla con string vacío" $
      shouldFail (parseRunTime "" "")

  -- ----------------------------------------------------------------
  describe "parsePBExp" $ do

    it "parsea bernoulli 1/2" $
      parsePBExp "" "<1/2>"
        `shouldParseTo` Ber (1/2)

    it "parsea bernoulli 0" $
      parsePBExp "" "<0>"
        `shouldParseTo` Ber 0

    it "parsea bernoulli 1" $
      parsePBExp "" "<1>"
        `shouldParseTo` Ber 1

    it "parsea bernoulli 1/3" $
      parsePBExp "" "<1/3>"
        `shouldParseTo` Ber (1/3)

    it "parsea bernoulli 3/4" $
      parsePBExp "" "<3/4>"
        `shouldParseTo` Ber (3/4)

    it "parsea bernoulli entero" $
      parsePBExp "" "<2>"
        `shouldParseTo` Ber 2

    it "parsea bernoulli fracción pequeña" $
      parsePBExp "" "<1/10>"
        `shouldParseTo` Ber (1/10)

    it "parsea bernoulli 2/3" $
      parsePBExp "" "<2/3>"
        `shouldParseTo` Ber (2/3)

    it "falla sin ángulos" $
      shouldFail (parsePBExp "" "1/2")

    it "falla con string vacío" $
      shouldFail (parsePBExp "" "")

  -- ----------------------------------------------------------------
  describe "parsePAExp" $ do

    it "parsea distribución singular" $
      parsePAExp "" "1/2 * <x>"
        `shouldParseTo` [(1/2, Var "x")]

    it "parsea suma de distribuciones" $
      parsePAExp "" "1/2 * <x> + 1/2 * <y>"
        `shouldParseTo` [(1/2, Var "x"), (1/2, Var "y")]

    it "parsea distribución con literal" $
      parsePAExp "" "1 * <0>"
        `shouldParseTo` [(1, Lit 0)]

    it "parsea distribución con fracción" $
      parsePAExp "" "1/3 * <x> + 2/3 * <y>"
        `shouldParseTo` [(1/3, Var "x"), (2/3, Var "y")]

    it "parsea tres puntos de distribución" $
      parsePAExp "" "1/3 * <x> + 1/3 * <y> + 1/3 * <z>"
        `shouldParseTo` [(1/3, Var "x"), (1/3, Var "y"), (1/3, Var "z")]

    it "parsea distribución con expresión aritmética" $
      parsePAExp "" "1/2 * <x + 1>"
        `shouldParseTo` [(1/2, Var "x" :+: Lit 1)]

    it "parsea con coeficiente entero" $
      parsePAExp "" "1 * <x>"
        `shouldParseTo` [(1, Var "x")]

    it "parsea coeficiente cero" $
      parsePAExp "" "0 * <x>"
        `shouldParseTo` [(0, Var "x")]

    it "falla sin coeficiente" $
      shouldFail (parsePAExp "" "<x>")

    it "falla con string vacío" $
      shouldFail (parsePAExp "" "")

  -- ----------------------------------------------------------------
  describe "parseProgram" $ do

    it "parsea skip" $
      parseProgram "" "skip"
        `shouldParseTo` Seq Empty Skip

    it "parsea empty" $
      parseProgram "" "empty"
        `shouldParseTo` Seq Empty Empty

    it "parsea asignación simple" $
      parseProgram "" "x := 1"
        `shouldParseTo` Seq Empty (Set "x" (Lit 1))

    it "parsea asignación probabilista" $
      parseProgram "" "x :~ 1/2 * <0> + 1/2 * <1>"
        `shouldParseTo` Seq Empty (PSet "x" [(1/2, Lit 0), (1/2, Lit 1)])

    it "parsea secuencia de asignaciones" $
      parseProgram "" "x := 1; y := 2"
        `shouldParseTo` Seq (Seq Empty (Set "x" (Lit 1))) (Set "y" (Lit 2))

    it "parsea if-else" $
      parseProgram "" "if (x <= y) {skip} else {skip}"
        `shouldParseTo` Seq Empty (If (Var "x" :<=: Var "y") (Seq Empty Skip) (Seq Empty Skip))

--    it "parsea it (azúcar sintáctica)" $
--      parseProgram "" "it (x <= y) {skip}"
--        `shouldParseTo` Seq Empty (Imp.it (Var "x" :<=: Var "y") (Seq Empty Skip))

    it "parsea while con invariante" $
      parseProgram "" "while (x <= y) {inv = x} {skip}"
        `shouldParseTo` Seq Empty (While (Var "x" :<=: Var "y") (Seq Empty Skip) (RunTimeArit (Var "x")))

    it "parsea for" $
      parseProgram "" "for (3) {x := 1}"
        `shouldParseTo` Seq Empty (for (Seq Empty (Set "x" (Lit 1))) 3)

    it "falla con string vacío" $
      shouldFail (parseProgram "" "")