module ImpVCGenSpec (spec) where

import Test.Hspec

import ImpVCGen hiding (runtime)
import Imp hiding (it)
import ImpParser
import Data.Either (fromRight)
import qualified Data.Set as Set

-- TODO: agrega los imports que falten (BExp, AExp, parser, etc.)

-- Helper para verificar que parsea correctamente
getRunTime :: String -> RunTime
getRunTime str = fromRight (error "parse error") (parseRunTime "" str)

spec :: Spec
spec = do

  describe "restrictionsToImplications" $ do

    context "while(x > 0){inv = a ++ 2**[x>0]**(x + c )}{x:= x-1}" $ do
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

  -- Programas de prueba reutilizados por varios describe de más abajo.
  --
  --   progAssign: x := y                                    (sin loops)
  --   progPSet:   x :~ <y>                                   (sin loops, PSet)
  --   progIf:     if (x > 0) { y := 1 } else { y := 2 }       (sin loops)
  --   progLoop:   while (x > 0) { x := x - 1 } [inv = a**x]   (1 loop, template "a")
  --   progNested: while (x > 0) {                             (2 loops anidados,
  --                 while (y > 0) { y := y - 1 } [inv = c**y]  templates "a"/"c"
  --                 ; x := x - 1
  --               } [inv = a**x]
  let progAssign = Set "x" (Var "y")
      progPSet   = PSet "x" (Imp.dirac (Var "y"))
      progIf     = If (Var "x" >: Lit 0) (Set "y" (Lit 1)) (Set "y" (Lit 2))
      progLoop   = While (Var "x" >: Lit 0) (Set "x" (Var "x" -: Lit 1)) (rtVar "a" :**: rtVar "x")
      innerLoop  = While (Var "y" >: Lit 0) (Set "y" (Var "y" -: Lit 1)) (rtVar "c" :**: rtVar "y")
      progNested = While (Var "x" >: Lit 0) (Seq innerLoop (Set "x" (Var "x" -: Lit 1))) (rtVar "a" :**: rtVar "x")

  describe "getExistencialAndUniversalVars" $ do

    it "una asignación simple no aporta variables existenciales" $ do
      getExistencialAndUniversalVars progAssign
        `shouldBe` ([], ["x", "y"])

    -- Regresión: PSet x parit antes sólo aportaba "x", ignorando las
    -- variables libres de "parit" (ver freeVarsPAExp en Imp.hs). Con
    -- dirac (Var "y"), "y" debe aparecer como universal igual que en Set.
    it "una asignación probabilista (PSet) también aporta las variables libres de la distribución" $ do
      getExistencialAndUniversalVars progPSet
        `shouldBe` ([], ["x", "y"])

    it "una variable de template que no aparece en el programa queda como existencial" $ do
      let (exist, universal) = getExistencialAndUniversalVars progLoop
      exist `shouldBe` ["a"]
      -- El conjunto de variables universales es {"x"}; la lista puede traer
      -- "x" repetido (ver la nota en ImpVCGen.hs sobre getExistencialAndUniversalVars) —
      -- por eso se compara como Set en vez de por igualdad exacta de listas.
      Set.fromList universal `shouldBe` Set.fromList ["x"]

    it "un if sin loops no aporta variables existenciales" $ do
      let (exist, universal) = getExistencialAndUniversalVars progIf
      exist `shouldBe` []
      Set.fromList universal `shouldBe` Set.fromList ["x", "y"]

    it "dos loops anidados con templates de nombres distintos aportan ambas variables de template" $ do
      let (exist, universal) = getExistencialAndUniversalVars progNested
      Set.fromList exist `shouldBe` Set.fromList ["a", "c"]
      Set.fromList universal `shouldBe` Set.fromList ["x", "y"]

  describe "relevantVars" $ do

    it "descarta variables que no aparecen en ninguna fórmula" $ do
      let formulaes = [ Implication { hypothesis = [Var "x" :<=: Lit 0]
                                     , conclusion = Var "a" :<=: Lit 0 } ]
      relevantVars formulaes ["a", "b", "x", "y"] `shouldBe` Set.fromList ["a", "x"]

    it "lista vacía de variables candidatas da un Set vacío" $ do
      let formulaes = [ Implication { hypothesis = [], conclusion = Var "a" :<=: Lit 0 } ]
      relevantVars formulaes [] `shouldBe` Set.empty

    it "sin fórmulas, ninguna variable es relevante" $ do
      relevantVars [] ["a", "b"] `shouldBe` Set.empty

  describe "programToSolverInput" $ do

    it "un programa sin loops no genera restricciones (pero sí clasifica sus variables)" $ do
      -- existential/for_all salen de getExistencialAndUniversalVars sobre
      -- TODO el programa, no de las restricciones — por eso "x"/"y" igual
      -- aparecen en for_all aunque no haya ningún while que resolver.
      let si = programToSolverInput progAssign
      solver_formulaes si `shouldBe` []
      existential si `shouldBe` Set.empty
      for_all si `shouldBe` Set.fromList ["x", "y"]

    it "un solo while junta su template como existencial y su variable de guarda como universal" $ do
      let si = programToSolverInput progLoop
      existential si `shouldBe` Set.fromList ["a"]
      for_all si `shouldBe` Set.fromList ["x"]
      -- 2 contextos posibles para la única condición del while (x > 0 / ¬(x > 0))
      length (solver_formulaes si) `shouldBe` 2

    it "dos loops anidados generan un único SolverInput' con todas las implicaciones juntas" $ do
      let si = programToSolverInput progNested
      existential si `shouldBe` Set.fromList ["a", "c"]
      for_all si `shouldBe` Set.fromList ["x", "y"]
      -- una restricción por loop, 2 implicaciones cada una (1 condición por loop)
      length (solver_formulaes si) `shouldBe` 4

  describe "programToSolverInputs" $ do

    it "un programa sin loops no genera ningún SolverInput'" $ do
      programToSolverInputs progAssign `shouldBe` []

    it "un solo while genera exactamente un SolverInput', igual al de programToSolverInput" $ do
      programToSolverInputs progLoop `shouldBe` [programToSolverInput progLoop]

    it "dos loops anidados generan dos SolverInput' independientes, uno por invariante" $ do
      let sis = programToSolverInputs progNested
      length sis `shouldBe` 2
      -- Cada SolverInput' individual tiene 2 implicaciones (1 condición, 2 contextos)
      map (length . solver_formulaes) sis `shouldBe` [2, 2]
      -- Por cómo vcGenerator encadena la continuación de un while dentro del
      -- otro (ver comentario de programToSolverInputs), en este ejemplo
      -- ambos invariantes terminan dependiendo de las dos variables de
      -- template y de las dos variables de programa — no es que el filtrado
      -- por relevantVars no esté funcionando, es una dependencia real.
      map existential sis `shouldBe` [Set.fromList ["a", "c"], Set.fromList ["a", "c"]]
      map for_all sis `shouldBe` [Set.fromList ["x", "y"], Set.fromList ["x", "y"]]
