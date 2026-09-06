module ImpIOSpec (spec) where

import Test.Hspec
import System.IO.Silently (capture)
import Data.Either (fromRight)
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import Imp hiding (it)
import ImpParser (parseProgram)
import ImpVCGen
import ImpSBV (runModel')
import ImpIO
import qualified ImpProgram as P

{-
  Tests del flujo de impresión modo nuevo (ImpIO.hs: showModel'/showSolverInput'/
  showSolverInputs'/completeRoutine'). Hasta esta sesión no existía ningún test
  automatizado para ImpIO.hs — sólo se había verificado a mano con scripts sueltos.

  Como estas funciones imprimen por stdout, se usa `capture` (paquete `silently`,
  agregado como dependencia de test en package.yaml/runtime.cabal) para
  redirigir y devolver lo impreso como String, sin ensuciar la salida de la
  suite ni depender de reimplementar a mano la redirección de Handles.

  No se fijan valores numéricos exactos de contraejemplos/testigos en los
  asserts (ej. no se hardcodea "x = 0.5") — eso depende de qué modelo
  concreto elija Z3 entre varios igual de válidos, y no está garantizado que
  sea siempre el mismo. Se verifica en cambio que el mensaje correspondiente
  y el nombre de la variable efectivamente se imprimieron.
-}

getProgram :: String -> Program
getProgram src = deepSimplifyProgram (fromRight (error ("no parsea: " ++ src)) (parseProgram "<test>" src))

-- Programas con variables de template, construidos a mano (no vía parser:
-- "a**x", variable por variable, no es sintaxis concreta soportada — ver
-- CLAUDE.md, sección de RunTime).
templateConTestigo :: Program
templateConTestigo = While False' Skip (rtVar "a")

templateSinTestigo :: Program
templateSinTestigo = While (Var "x" >: Lit 0) (Set "x" (Var "x" -: Lit 1)) (rtVar "a" :**: rtVar "x")

-- Cpvc (informe, Anexo C.1.8): pwhile con un while anidado en su cuerpo, con
-- los dos invariantes dejados como plantilla. Los dos ciclos comparten
-- variables de template entre sus obligaciones, así que es el caso que
-- obliga a resolverlas juntas (ver ImpIO.sharedExistentials).
cpvcTemplate :: String
cpvcTemplate = "pwhile(<9/10>) {pinv = a0 ++ a1**[c!=1] ++ a2**[c==1]}"
            ++ "{while(c == 1){inv = b0 ++ b1**[c!=1] ++ b2**[c==1]} {c:~ 1/2* <0> + 1/2* <1>}}"

spec :: Spec
spec = do

  describe "showSolverInput'" $ do

    it "invariante concreto inválido (p2_1) -> False, imprime el contraejemplo" $ do
      let [si] = programToSolverInputs (getProgram P.p2_1)
      (output, valid) <- capture (showSolverInput' 1 si)
      valid `shouldBe` False
      output `shouldContain` "no es válida"
      output `shouldContain` "Un contraejemplo encontrado es:"
      output `shouldContain` "x ="

    it "invariante concreto válido (cdkcMas) -> True, sin contraejemplo ni testigo" $ do
      let [si] = programToSolverInputs (getProgram P.cdkcMas)
      (output, valid) <- capture (showSolverInput' 1 si)
      valid `shouldBe` True
      output `shouldContain` "es válida."
      output `shouldNotContain` "contraejemplo"
      output `shouldNotContain` "Testigo"

    it "template con testigo -> True, imprime el testigo encontrado para 'a'" $ do
      let [si] = programToSolverInputs templateConTestigo
      (output, valid) <- capture (showSolverInput' 1 si)
      valid `shouldBe` True
      output `shouldContain` "Testigo encontrado para las variables de template:"
      output `shouldContain` "a ="

    it "template sin testigo -> False, sin pretender mostrar un contraejemplo" $ do
      let [si] = programToSolverInputs templateSinTestigo
      (output, valid) <- capture (showSolverInput' 1 si)
      valid `shouldBe` False
      output `shouldContain` "no existe una asignación de las variables de template que la satisfaga"
      output `shouldNotContain` "contraejemplo"

  describe "showSolverInputs'" $ do

    it "programa sin ciclos -> True, sin listar obligaciones" $ do
      (output, valid) <- capture (showSolverInputs' (programToSolverInputs (getProgram P.p1_1)))
      valid `shouldBe` True
      output `shouldContain` "no hay obligaciones de prueba asociadas"

    it "todas las obligaciones válidas (cpkcMas, 3 pwhile en secuencia) -> True" $ do
      (output, valid) <- capture (showSolverInputs' (programToSolverInputs (getProgram P.cpkcMas)))
      valid `shouldBe` True
      output `shouldContain` "las obligaciones de prueba son válidas"
      -- las 3 obligaciones se imprimieron, no sólo la primera
      output `shouldContain` "[1]"
      output `shouldContain` "[2]"
      output `shouldContain` "[3]"

    it "alguna obligación inválida (cpkcMenos) -> False" $ do
      (output, valid) <- capture (showSolverInputs' (programToSolverInputs (getProgram P.cpkcMenos)))
      valid `shouldBe` False
      output `shouldContain` "alguna obligación de prueba no es válida"
      output `shouldContain` "Ajuste los invariantes"

  describe "completeRoutine'" $ do

    it "programa sin ciclos: encabezado, tiempo calculado y veredicto final" $ do
      (output, ()) <- capture (completeRoutine' (getProgram P.p1_1) P.p1_1)
      output `shouldContain` "Programa Analizado:"
      output `shouldContain` P.p1_1
      output `shouldContain` "Tiempo de ejecución calculado:"
      output `shouldContain` "no hay obligaciones de prueba asociadas"
      output `shouldContain` "Análisis Finalizado."

    it "invariante concreto inválido (p2_1): mismo veredicto que el modo antiguo" $ do
      (output, ()) <- capture (completeRoutine' (getProgram P.p2_1) P.p2_1)
      output `shouldContain` "no es válida"
      output `shouldContain` "Un contraejemplo encontrado es:"
      output `shouldContain` "Ajuste los invariantes"
      output `shouldContain` "Análisis Finalizado."

    it "template con testigo (while(false){inv=a}{skip}): reporta válido con el testigo" $ do
      (output, ()) <- capture (completeRoutine' templateConTestigo "while(false){inv = a}{skip}")
      output `shouldContain` "es válida."
      output `shouldContain` "Testigo encontrado"
      output `shouldContain` "Análisis Finalizado."

    it "templates compartidos entre obligaciones (Cpvc anidado): resuelve todo junto, un solo testigo" $ do
      -- Regresión del arreglo de sharedExistentials: antes se resolvía una
      -- obligación por vez y cada una elegía sus propios valores para las
      -- mismas variables de template, reportando "todas válidas" con testigos
      -- contradictorios entre sí (a1 = 1 en una, a1 = -1 en la otra).
      (output, ()) <- capture (completeRoutine' (getProgram cpvcTemplate) cpvcTemplate)
      output `shouldContain` "comparten variables de template"
      output `shouldContain` "se resuelven todas juntas"
      -- Un único bloque de obligación (el sistema completo), no uno por ciclo.
      length (filter ("Obligación de prueba" `isPrefixOf`) (lines output)) `shouldBe` 1
      output `shouldContain` "Testigo encontrado"
      output `shouldContain` "Análisis Finalizado."

  describe "sharedExistentials" $ do

    it "sin variables de template compartidas devuelve el conjunto vacío" $
      sharedExistentials (programToSolverInputs templateConTestigo) `shouldBe` Set.empty

    it "dos ciclos anidados con templates propios comparten variables entre sus obligaciones" $ do
      let compartidas = sharedExistentials (programToSolverInputs (getProgram cpvcTemplate))
      compartidas `shouldSatisfy` not . Set.null
      -- Son variables de template (las "a*"/"b*" de los invariantes), no
      -- variables de programa: "c" nunca puede aparecer acá.
      compartidas `shouldSatisfy` Set.notMember "c"

  describe "showModel'" $ do

    it "imprime cada nombre con su valor y la palabra 'Real'" $ do
      let [si] = programToSolverInputs templateConTestigo
      result <- runModel' si
      (output, ()) <- capture (showModel' result (Set.toList (existential si)))
      output `shouldContain` "a ="
      output `shouldContain` "Real"

    it "lista vacía de nombres no imprime nada" $ do
      let [si] = programToSolverInputs templateConTestigo
      result <- runModel' si
      (output, ()) <- capture (showModel' result [])
      output `shouldBe` ""
