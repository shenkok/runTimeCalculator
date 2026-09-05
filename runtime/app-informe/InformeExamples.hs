module Main where

import Imp (deepSimplifyProgram)
import ImpParser (parseProgram)
import ImpIO (completeRoutine')
import qualified ImpProgram as P

{-
  Corre completeRoutine' (el modo NUEVO de impresión: un SolverInput' por
  invariante, cuantificación ∃∀ real vía mkUniversales — ver CLAUDE.md,
  sección "Flujo de impresión, modo nuevo") sobre los programas de prueba
  del informe (memoria de título), banco definido en ImpProgram.hs.

  IMPORTANTE: la sintaxis concreta de una indicatriz ponderada cambió esta
  sesión — el viejo "[b]<>algo" ya no existe, ahora una indicatriz
  ponderada se escribe "[b]**algo" (multiplicación genérica, ver CLAUDE.md,
  sección "RunTime: indicatriz como constructor propio (RunTimeBExp)"). Los
  strings de abajo se importan directo de ImpProgram.hs, que ya está
  migrado a la sintaxis nueva — si agregás un ejemplo propio acá, escribí
  el invariante con "**", no con "<>".

  Para ejecutar:
    cd runtime
    cabal run informe-examples     -- o
    stack exec informe-examples    (tras "stack build")
-}

-- | Imprime un encabezado con el ejemplo (y su cita en el informe, cuando
-- aplica) y corre completeRoutine' sobre el programa parseado.
runExample :: String -> String -> IO ()
runExample label input = do
  putStrLn (replicate 100 '=')
  putStrLn label
  putStrLn (replicate 100 '=')
  case parseProgram "<informe>" input of
    Left err      -> print err
    Right program -> completeRoutine' (deepSimplifyProgram program) input
  putStrLn ""

main :: IO ()
main = mapM_ (uncurry runExample)
  [ -- Programas deterministicos sin ciclo (siempre válidos, sin obligaciones)
    ("Cdks / cdks (informe pp. 42-43, Fig. 5.2/5.3, Listing 5.1)", P.cdks)
  , ("Cdvs / cdvs (informe pp. 66-67, Anexo C.1.1, Fig. C.1/C.2)", P.cdvs)
  , ("p1_1", P.p1_1)
  , ("p1_2", P.p1_2)
  , ("p1_3", P.p1_3)
  , ("p1_4", P.p1_4)

    -- Programas deterministicos con ciclo
  , ("Cdkc- / cdkcMenos (informe pp. 44-46, Fig. 5.4/5.5, Listing 5.2) -- invariante\n\
     \  insuficiente para x racional, el informe prueba a mano el contraejemplo x = 1/2", P.cdkcMenos)
  , ("Cdkc+ / cdkcMas (informe p. 68, Anexo C.1.2, Fig. C.3/C.4) -- mismo invariante\n\
     \  con holgura '+1', válido", P.cdkcMas)
  , ("p2_1 -- mismo invariante insuficiente que Cdkc-, sin el 'x:=3' previo", P.p2_1)
  , ("p2_2 -- invariante con guarda 'y>=10', válido", P.p2_2)
  , ("Cdvc+ / cdvcMas (informe Anexo C.1.3, pp. 71-72) -- guarda compuesta\n\
     \  'y<=x && x<=z', válido", P.cdvcMas)
  , ("Cdvc- / cdvcMenos (informe pp. 73-74, Fig. C.7/C.8, Anexo C.1.4) -- OJO: la\n\
     \  heurística de variables existenciales/universales clasifica 'x' como EXISTENCIAL\n\
     \  acá (no aparece asignada ni en ninguna guarda real -la guarda del while es la\n\
     \  constante 'false'-), así que este modo responde 'válido' con testigo x=1, al\n\
     \  revés de lo que el informe prueba a mano (contraejemplos x=-1 y x=0). No es un\n\
     \  bug de esta corrida puntual: es la limitación ya documentada de\n\
     \  getExistencialAndUniversalVars en CLAUDE.md.", P.cdvcMenos)

    -- Programas probabilísticos sin ciclo (siempre válidos, sin obligaciones)
  , ("Cpks / cpks (informe Anexo C.1.5)", P.cpks)
  , ("Cpvs / cpvs (informe Anexo C.1.6, Fig. C.11)", P.cpvs)
  , ("cTrunc", P.cTrunc)

    -- Programas probabilísticos con ciclo
  , ("p4_1 -- pwhile trivial, válido", P.p4_1)
  , ("Cpkc+ / cpkcMas -- tres pwhile en secuencia con invariantes fijos exactos\n\
     \  (9, 6, 3), todos válidos", P.cpkcMas)
  , ("Cpkc- / cpkcMenos (informe pp. 78-79, Fig. C.13/C.14, Anexo C.1.7) -- for(3){pwhile\n\
     \  (pinv=3)}, 2 obligaciones inválidas (9/2<=3) y 1 válida (3<=3) => no válido en\n\
     \  conjunto", P.cpkcMenos)
  , ("p4_2 -- while(c==1){inv=1+4*[c==1]}, válido", P.p4_2)
  , ("p4_6 -- mismo caso que p4_2: K=4 es la cota exacta, válido", P.p4_6)
  , ("p4_8 -- K=3 subestima la cota exacta (4), no válido", P.p4_8)
  , ("p4_9 -- K=5 sobreestima la cota exacta pero sigue siendo válida (holgada)", P.p4_9)
  , ("p4_3 -- pwhile anidando un while, alguna obligación no válida", P.p4_3)
  , ("cpvcMas (informe Anexo C.1.8, pp. 80-82) -- aproximación del invariante I2 que\n\
     \  deja ambas obligaciones válidas", P.cpvcMas)
  , ("cpvcMenos (informe Anexo C.1.8, I2^min) -- otra aproximación del denominador,\n\
     \  deja el while interno no válido", P.cpvcMenos)
  , ("cpvc -- variante base, sin sufijo Mas/Menos, alguna obligación no válida", P.cpvc)

    -- Precisión numérica: invariantes con constantes racionales muy cercanas al límite
  , ("p4_10 -- holgura entera '+2', válido", P.p4_10)
  , ("p4_14 -- holgura entera '+1', válido", P.p4_14)
  , ("p4_11 -- holgura fraccionaria mínima (2439/2438), válido", P.p4_11)
  , ("p4_13 -- holgura fraccionaria mínima (7721/7720), válido", P.p4_13)
  , ("p4_15 -- holgura fraccionaria mínima (7721/7720), válido", P.p4_15)

    -- Invariante-plantilla explícito (variable existencial genuina, "A")
  , ("p4_7 -- usa 'A' como invariante-plantilla explícito: acá SÍ hay una variable\n\
     \  existencial genuina (no un caso confundido por la heurística)", P.p4_7)
  ]
