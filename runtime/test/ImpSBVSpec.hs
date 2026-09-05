module ImpSBVSpec (spec) where

import Test.Hspec
import Data.SBV
import qualified Data.Map as M
import qualified Data.Set as Set
import ImpSBV

{-
  Tests de mkUniversales. A diferencia del resto de la suite, estos SÍ
  invocan a Z3 (isSatisfiable corre el solver de verdad) — mkUniversales
  produce un SBool opaco, no hay forma de inspeccionar el cuantificador que
  arma sin resolverlo. Se mantienen las fórmulas chicas (pocas variables,
  aritmética lineal) para que sigan siendo rápidos.
-}

spec :: Spec
spec = describe "mkUniversales" $ do

  it "sin variables universales, un objetivo trivialmente verdadero es satisfacible" $ do
    isSatisfiable (constrain (mkUniversales [] (const sTrue)) :: Symbolic ())
      `shouldReturn` True

  it "una sola variable universal fuerza que la fórmula valga para TODO x, no sólo para alguno" $ do
    -- "para todo x real, x > 0" es falso (x = -1 lo viola) — si mkUniversales
    -- tratara "x" como existencial en vez de universal, esto saldría
    -- satisfacible (con x > 0 alcanza cualquier x positivo).
    isSatisfiable (constrain (mkUniversales ["x"] (\env -> env M.! "x" .> 0)) :: Symbolic ())
      `shouldReturn` False

  it "más de 3 variables universales ya no falla (regresión del tope viejo de mkUniversales)" $ do
    -- Antes: `case length names of 0/1/2/3 -> ...; _ -> error "Máximo 3..."`.
    -- Con 5 nombres esto tiraba error en vez de construir la fórmula.
    let act = do
          a <- sReal "a"
          constrain $ mkUniversales ["x1", "x2", "x3", "x4", "x5"] $ \env ->
            sAnd [ env M.! n .>= 0 .=> a .<= 0 | n <- ["x1", "x2", "x3", "x4", "x5"] ]
    isSatisfiable act `shouldReturn` True

  it "cada nombre recibe su propia variable universal, no se alían entre sí" $ do
    -- "para todo x,y reales, x == y" es falso (x=0,y=1 lo viola). Si "x" y
    -- "y" terminaran atadas al mismo símbolo por error, esto sería
    -- trivialmente satisfacible (todo término es igual a sí mismo).
    isSatisfiable (constrain (mkUniversales ["x", "y"] (\env -> env M.! "x" .== env M.! "y")) :: Symbolic ())
      `shouldReturn` False

  it "combina una variable existencial con variables universales (∃a. ∀x. x>=0 => a<=0)" $ do
    let act = do
          a <- sReal "a"
          constrain $ mkUniversales ["x"] $ \env -> env M.! "x" .>= 0 .=> a .<= 0
    isSatisfiable act `shouldReturn` True

  it "el Map que recibe la función callback trae exactamente los nombres pedidos, ni más ni menos" $ do
    let names = ["p", "q", "r"]
        act = constrain $ mkUniversales names $ \env ->
                literal (Set.fromList (M.keys env) == Set.fromList names)
    isSatisfiable (act :: Symbolic ()) `shouldReturn` True
