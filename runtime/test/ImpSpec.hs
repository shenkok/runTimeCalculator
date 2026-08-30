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

  describe "normBExp" $ do

    -- Átomos: forma canónica "diferencia contra cero", independiente de
    -- cómo se escribió la comparación.
    it "constante true no cambia" $ do
      normBExp True' `shouldBe` True'

    it "constante false no cambia" $ do
      normBExp False' `shouldBe` False'

    it "atomo <= se lleva a diferencia contra cero" $ do
      normBExp (Var "x" :<=: Var "y")
        `shouldBe` (completeNormArit (Var "x" -: Var "y") :<=: Lit 0)

    it "atomo == se lleva a diferencia contra cero" $ do
      normBExp (Var "x" :==: Var "y")
        `shouldBe` (completeNormArit (Var "x" -: Var "y") :==: Lit 0)

    it "x <= y y su equivalente y >= x normalizan igual" $ do
      normBExp (Var "x" :<=: Var "y") `shouldBe` normBExp (Var "y" >=: Var "x")

    it "reescritura algebraica del mismo átomo normaliza igual: x+1<=y+1 vs x<=y" $ do
      normBExp ((Var "x" :+: Lit 1) :<=: (Var "y" :+: Lit 1))
        `shouldBe` normBExp (Var "x" :<=: Var "y")

    -- NNF: Not se empuja hasta las hojas (De Morgan), nunca queda envolviendo
    -- un :&:/:|:.
    it "doble negación se cancela" $ do
      normBExp (Not (Not (Var "x" :<=: Lit 0)))
        `shouldBe` normBExp (Var "x" :<=: Lit 0)

    it "De Morgan sobre &&" $ do
      let a = Var "a" :<=: Lit 0
          b = Var "b" :<=: Lit 0
      normBExp (Not (a :&: b)) `shouldBe` (Not (normBExp a) :|: Not (normBExp b))

    it "De Morgan sobre ||" $ do
      let a = Var "a" :<=: Lit 0
          b = Var "b" :<=: Lit 0
      normBExp (Not (a :|: b)) `shouldBe` (Not (normBExp a) :&: Not (normBExp b))

    it "Not True' es False'" $ do
      normBExp (Not True') `shouldBe` False'

    it "Not False' es True'" $ do
      normBExp (Not False') `shouldBe` True'

    -- Asociatividad/conmutatividad: el orden y la asociación con la que se
    -- escribe una conjunción/disyunción no afecta la forma normal.
    it "reasocia y reordena una conjunción de tres átomos" $ do
      let a = Var "a" :<=: Lit 0
          b = Var "b" :<=: Lit 0
          c = Var "c" :<=: Lit 0
      normBExp ((a :&: b) :&: c) `shouldBe` normBExp ((c :&: b) :&: a)

    it "reasocia y reordena una disyunción de tres átomos" $ do
      let a = Var "a" :<=: Lit 0
          b = Var "b" :<=: Lit 0
          c = Var "c" :<=: Lit 0
      normBExp (a :|: (b :|: c)) `shouldBe` normBExp ((c :|: a) :|: b)

    -- Idempotencia
    it "a && a colapsa a a" $ do
      let a = Var "a" :<=: Lit 0
      normBExp (a :&: a) `shouldBe` normBExp a

    it "a || a colapsa a a" $ do
      let a = Var "a" :<=: Lit 0
      normBExp (a :|: a) `shouldBe` normBExp a

    it "(a||b) && (b||a) colapsa a a||b" $ do
      let a = Var "a" :<=: Lit 0
          b = Var "b" :<=: Lit 0
      normBExp ((a :|: b) :&: (b :|: a)) `shouldBe` normBExp (a :|: b)

    it "dos disyuntos idénticos tras normalizar (aunque estén escritos distinto) colapsan" $ do
      let a = Var "a" :<=: Lit 0
          b = Var "x" :<=: Var "y"
          c = Var "c" :==: Lit 0
      normBExp (((b :&: a) :&: c) :|: ((c :&: b) :&: a))
        `shouldBe` normBExp (a :&: b :&: c)

    -- Complementación
    it "a && !a es False'" $ do
      let a = Var "a" :<=: Lit 0
      normBExp (a :&: Not a) `shouldBe` False'

    it "a || !a es True'" $ do
      let a = Var "a" :<=: Lit 0
      normBExp (a :|: Not a) `shouldBe` True'

    -- Neutros / absorbentes
    it "a && True' colapsa a a" $ do
      let a = Var "a" :<=: Lit 0
      normBExp (a :&: True') `shouldBe` normBExp a

    it "a || False' colapsa a a" $ do
      let a = Var "a" :<=: Lit 0
      normBExp (a :|: False') `shouldBe` normBExp a

    it "a && False' es False'" $ do
      let a = Var "a" :<=: Lit 0
      normBExp (a :&: False') `shouldBe` False'

    it "a || True' es True'" $ do
      let a = Var "a" :<=: Lit 0
      normBExp (a :|: True') `shouldBe` True'

    -- Azúcar sintáctica (<, >, /=): se desazucaran con Not por debajo, deben
    -- normalizar de forma consistente con los átomos primitivos.
    it "x < y normaliza usando Not sobre el átomo canónico y <= x" $ do
      normBExp (Var "x" <: Var "y")
        `shouldBe` Not (completeNormArit (Var "y" -: Var "x") :<=: Lit 0)

    it "x /= y normaliza usando Not sobre el átomo canónico x == y" $ do
      normBExp (Var "x" /=: Var "y")
        `shouldBe` Not (completeNormArit (Var "x" -: Var "y") :==: Lit 0)

    -- Fuera de alcance a propósito: normBExp NO calcula DNF/CNF, así que no
    -- aplica distributividad/absorción entre cláusulas distintas aunque sean
    -- equivalentes proposicionalmente (acá "(X && c) || (X && !c)" sería
    -- lógicamente igual a X, pero normBExp no lo reduce, sólo normaliza cada
    -- cláusula por separado). Ver comentario de normBExp en Imp.hs.
    it "NO reduce por distributividad entre cláusulas (fuera de alcance)" $ do
      let a = Var "a" :<=: Lit 0
          b = Var "x" :<=: Var "y"
          c = Var "c" :==: Lit 0
      normBExp (((a :&: b) :&: Not c) :|: (c :&: (b :&: a)))
        `shouldNotBe` normBExp (a :&: b)

  describe "simplifyRunTime / deepSimplifyRunTime" $ do

    -- Regla 1: [b]<>r1 ++ [b]<>r2 = [b]<>(r1 ++ r2) cuando "b" es
    -- sintácticamente igual en ambos sumandos. Como r1/r2 son RunTimeArit
    -- puros, la regla de suma de arit ya existente termina de colapsarlos en
    -- un solo RunTimeArit — por eso el resultado esperado usa
    -- completeNormArit en vez de dejar un :++: sin reducir.
    it "fusiona dos indicatrices iguales en una suma" $ do
      let b = Var "x" :<=: Lit 0
      deepSimplifyRunTime ((b :<>: rtVar "y") :++: (b :<>: rtOne))
        `shouldBe` (b :<>: RunTimeArit (completeNormArit (Var "y" :+: Lit 1)))

    it "fusiona indicatriz igual cuando encabeza una cadena de :++: más larga" $ do
      let b = Var "x" :<=: Lit 0
      deepSimplifyRunTime ((b :<>: rtVar "y") :++: ((b :<>: rtOne) :++: rtVar "z"))
        `shouldBe` ((b :<>: RunTimeArit (completeNormArit (Var "y" :+: Lit 1))) :++: rtVar "z")

    it "NO fusiona indicatrices distintas (fuera de alcance: eso requeriría normBExp)" $ do
      let b1 = Var "x" :<=: Lit 0
          b2 = Var "y" :<=: Lit 0
      deepSimplifyRunTime ((b1 :<>: rtVar "y") :++: (b2 :<>: rtOne))
        `shouldBe` ((b1 :<>: rtVar "y") :++: (b2 :<>: rtOne))

    -- Regla 2: reasociación de :++: hacia la derecha, para que la regla 1 (y
    -- las reglas de RunTimeArit ya existentes) encuentren términos
    -- adyacentes sin importar cómo se parentizó la suma original. Se usan
    -- indicatrices con condiciones distintas (no fusionables) para que el
    -- test discrimine reasociación pura, sin mezclarse con la regla 1.
    it "reasocia una cadena de :++: parentizada a la izquierda (sin fusión posible)" $ do
      let b1 = Var "x" :<=: Lit 0
          b2 = Var "y" :<=: Lit 0
      deepSimplifyRunTime (((b1 :<>: rtVar "y") :++: (b2 :<>: rtOne)) :++: rtVar "z")
        `shouldBe` ((b1 :<>: rtVar "y") :++: ((b2 :<>: rtOne) :++: rtVar "z"))

    it "reasociación deja fusionar dos indicatrices iguales aunque queden a la izquierda" $ do
      let b = Var "x" :<=: Lit 0
      deepSimplifyRunTime (((b :<>: rtVar "y") :++: (b :<>: rtOne)) :++: rtVar "z")
        `shouldBe` ((b :<>: RunTimeArit (completeNormArit (Var "y" :+: Lit 1))) :++: rtVar "z")

    -- Regla 3: fusión de ponderaciones :**: anidadas. Se envuelve un [b]<>x
    -- (en vez de un AExp puro) porque un AExp puro ya colapsa a través de la
    -- regla previa "k :**: RunTimeArit arit" antes de que la fusión nueva
    -- tenga oportunidad de aplicarse — así el test sí ejercita la regla 3.
    it "fusiona dos ponderaciones anidadas sobre una indicatriz en una sola" $ do
      let b = Var "x" :<=: Lit 0
      deepSimplifyRunTime (2 :**: (3 :**: (b :<>: rtVar "y")))
        `shouldBe` (6 :**: (b :<>: rtVar "y"))

    it "ponderación anidada que colapsa a 0 da rtZero" $ do
      deepSimplifyRunTime (0 :**: (3 :**: rtVar "x"))
        `shouldBe` rtZero

    it "ponderación anidada que colapsa a 1 deja el runtime sin envolver" $ do
      deepSimplifyRunTime (1 :**: (1 :**: rtVar "x"))
        `shouldBe` rtVar "x"

    -- Regla 1b: [b]<>r ++ [!b]<>r = r (caso seguro de "suma de indicatrices",
    -- válido para cualquier b porque b y ¬b siempre son excluyentes).
    it "fusiona indicatrices complementarias con el mismo peso en 1*r" $ do
      let b = Var "x" :<=: Lit 0
      deepSimplifyRunTime ((b :<>: rtVar "y") :++: (Not b :<>: rtVar "y"))
        `shouldBe` rtVar "y"

    it "NO fusiona indicatrices complementarias si el peso es distinto" $ do
      let b = Var "x" :<=: Lit 0
      deepSimplifyRunTime ((b :<>: rtVar "y") :++: (Not b :<>: rtVar "z"))
        `shouldBe` ((b :<>: rtVar "y") :++: (Not b :<>: rtVar "z"))

    it "fusiona indicatrices complementarias cuando la segunda encabeza una cadena" $ do
      let b = Var "x" :<=: Lit 0
      deepSimplifyRunTime ((b :<>: rtVar "y") :++: ((Not b :<>: rtVar "y") :++: rtVar "z"))
        `shouldBe` RunTimeArit (completeNormArit (Var "y" :+: Var "z"))

    -- Regla 4: el producto de indicatrices es la indicatriz de la
    -- conjunción, [b1]*[b2] = [b1 && b2] — a diferencia de la 1b, esto vale
    -- siempre, sin necesidad de que b1/b2 sean complementarias ni nada.
    it "fusiona indicatrices anidadas en la conjunción de sus condiciones" $ do
      let b1 = Var "p" :<=: Lit 0
          b2 = Var "q" :<=: Lit 0
      deepSimplifyRunTime (b1 :<>: (b2 :<>: rtVar "z"))
        `shouldBe` ((b1 :&: b2) :<>: rtVar "z")

    it "fusiona una cadena de tres indicatrices anidadas" $ do
      let b1 = Var "p" :<=: Lit 0
          b2 = Var "q" :<=: Lit 0
          b3 = Var "r" :<=: Lit 0
      deepSimplifyRunTime (b1 :<>: (b2 :<>: (b3 :<>: rtVar "z")))
        `shouldBe` ((b1 :&: (b2 :&: b3)) :<>: rtVar "z")

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