# runTimeCalculator

## Qué es esto

Herramienta en Haskell que automatiza el cálculo del **tiempo de ejecución esperado** de
programas probabilísticos, usando la transformada `ert[·]` (Kaminski, Katoen, Matheja,
Olmedo — *Weakest Precondition Reasoning for Expected Run-Times of Probabilistic Programs*).
Además de calcular `ert[·]`, la herramienta:

- genera las **obligaciones de prueba** asociadas a los invariantes de los ciclos (`vcg[·]`,
  al estilo VCGen / lógica de Hoare), y
- las **verifica automáticamente** con el SMT-solver Z3 vía la librería `SBV`, reportando un
  contraejemplo cuando un invariante propuesto no es válido.

Este repo es la continuación de una memoria de título ya finalizada (ver **Contexto extenso**
abajo). El código actual está en pleno refactor de dos decisiones de diseño puntuales:
generalizar las expresiones aritméticas de *lineales* a *polinomiales*, y generalizar
`RunTime` de "ponderación por constante" a "multiplicación genuina entre RunTimes". Si vas a
tocar `Imp.hs`, lee las secciones "AExp: de lineal a polinomial" y "RunTime: de ponderación
por constante a multiplicación entre RunTimes" antes de asumir cómo debería comportarse.

**Estado actual de la rama** (`feature/change_arit_expression`): ambos refactores (`AExp` y
`RunTime`) están terminados y el proyecto compila y pasa sus tests de punta a punta, tanto con
`cabal` como con `stack` (ver sección **Build**) — `189 examples, 0 failures, 5 pending`. Los
5 `pending` son esqueletos de tests sin terminar en `ImpVCGenSpec.hs` (trabajo futuro, no
bugs). El constructor de indicatriz de `RunTime` también cambió esta sesión: ver "RunTime:
indicatriz como constructor propio (RunTimeBExp)" más abajo antes de tocar cualquier función
que mencione `:<>:`/indicatrices — ese constructor ya no existe.

## Contexto extenso (leer antes de decisiones de diseño no triviales)

- `INFORME_TRABAJO_DE_T_TULO_LUIS_PINOCHET_GONZALEZ.pdf` (raíz del repo): la memoria de
  título completa de la versión **lineal** de esta herramienta. Es la referencia normativa
  para: la gramática abstracta de `AExp`/`BExp`/`RunTime`/`Program`, la definición formal de
  `ert[·]` y `vcg[·]`, el algoritmo de linealización + cambio de cuantificador (∀→∃ por
  contradicción) que convierte una obligación de prueba en problemas para SBV, y el diseño de
  tests de validación (12 subcategorías: determinista/probabilista × ciclo/sin-ciclo ×
  invariante correcto/incorrecto). Sección **6.4 "Posibles trabajos futuros"** propone
  explícitamente el cambio que se está haciendo en esta rama (ver abajo).
- `material2020/Propuesta_Final_Luis_Pinochet.pdf`: la propuesta de investigación original
  (más teoría de fondo: por qué `ert[·]` sobre el enfoque de martingalas de Wang et al., rol
  de los invariantes, trabajos relacionados).
- `dummy.pdf` (raíz del repo, sin trackear en git — subido en esta sesión): otra copia del
  mismo informe (anonimizada, "DUMMY" en vez del nombre del autor/profesor guía; 95 páginas vs
  97 de `INFORME_..._GONZALEZ.pdf`, mismo contenido). Se usó para citar con precisión el
  **Capítulo 5 "Validación"** y el **Anexo C "Tests y demostraciones asociados a la
  Validación"** (págs. 37-88), que documentan a mano — con contraejemplos concretos — los 11
  programas de prueba de las 12 subcategorías (determinista/probabilista × sin-ciclo/con-ciclo
  × invariante correcto/incorrecto) que están codificados como strings en `ImpProgram.hs`.
  `test/ImpProgramSpec.hs` cita estas páginas explícitamente (ver sección "Tests con el banco
  de la memoria" más abajo).
- `README.md`: gramática concreta/abstracta resumida, mapa de módulos, instrucciones de build
  con `stack`, ejemplos de uso interactivo (`run "..."`, `fp`, `fpp`).

## Mapa de módulos (`runtime/src/`)

- **`Imp.hs`** — el núcleo: ASTs de `AExp`, `BExp`, `RunTime`, `PAExp`/`PBExp` (probabilistas),
  `Program`; azúcar sintáctica; sustitución; `freeVars`; simplificación y normalización de
  `AExp` (`normArit`, `simplifyArit`, `completeNormArit`); simplificación de `BExp`/`RunTime`/
  `Program`; `expectedValue`/`aexpE` para calcular esperanzas sobre distribuciones.
- **`ImpParser.hs`** — parser Parsec de la sintaxis concreta (`aexp`, `bexp`, `runtime`,
  `program`, etc.), espejo de la sintaxis abstracta de `Imp.hs`.
- **`ImpVCGen.hs`** — `vcg[·]`: recorre un `Program` y devuelve `(RunTime, [obligaciones])`;
  linealización de `RunTime`→`AExp` bajo hipótesis sobre las indicatrices (`getBExp`,
  contexto/restricción derivada); `restrictionsToImplications`/`programToSolverInput` arman
  el input final para SBV.
- **`ImpSBV.hs`** — traduce `AExp`/`BExp`/implicaciones a `SBV`; `mkUniversales` cuantifica una
  cantidad arbitraria de variables universales (ver sección "mkUniversales: de tope de 3 a
  cantidad arbitraria" más abajo — el tope duro de 3 que hubo en algún momento era un artefacto
  de la API tipada de SBV, no un límite real de Z3; sigue siendo cierto que aritmética real
  cuantificada es cara para *cualquier* cantidad de variables).
- **`ImpIO.hs`** — formatea el output para consola (tiempo calculado, obligaciones,
  contraejemplos); `completeRoutine` es el modo antiguo (`routineInput`/`restrictionsToSolver`,
  todo variable libre, un problema de SBV por contexto), `completeRoutine'` el modo nuevo
  (`programToSolverInputs`/`mkUniversales`, ∃∀ real, un problema por obligación) — ver sección
  "Flujo de impresión, modo nuevo" más abajo.
- **`ImpProgram.hs`** — banco de programas de ejemplo/test en sintaxis abstracta (`Ctrunc`,
  `Cgeo`, las 12 categorías `Cdks`/`Cpvc+`/etc. de la memoria).
- **`app/Main.hs`** — entry point interactivo: `run "<programa>"`, `fp`/`fpp` (iteración de
  punto fijo de Kleene sobre la función característica de un `while`/`pwhile`, útil para
  *encontrar* candidatos a invariante a mano, técnica descrita y usada extensamente en la
  memoria §5.4.3 y Anexo C.1.8).

## AExp: de lineal a polinomial (el cambio en curso en esta rama)

La memoria original (`INFORME_...pdf`) restringió `AExp` deliberadamente a **aritmética
lineal**: `AExp :*: AExp` no existía, sólo `Constant :*: AExp` (ponderación por constante).
La razón, citada textualmente del informe (§4.1.1):

> "El principal motivo de acotar AExp a este tipo de expresiones es la capacidad resolutiva
> de los smt-solvers... El tener expresiones no lineales... dificulta mucho los cálculos
> teóricos y, en consecuencia, la comparación entre la teoría y la automatización."

Esta rama (`feature/change_arit_expression`) implementa justo lo que el informe deja como
trabajo futuro en §6.4 ("Expansión del conjunto AExp"): generalizar el constructor a
`AExp :*: AExp` para poder representar productos genuinos (ej. `n * m` para loops anidados
con cotas variables), no sólo ponderaciones por constante.

**Trade-off que hay que tener presente**: `ImpSBV.hs` cuantifica variables universales con
`mkUniversales` sobre `AlgReal` (ver sección "mkUniversales: de tope de 3 a cantidad
arbitraria" más abajo — ya no tiene tope duro, pero eso no cambia el costo). Aritmética real no
lineal cuantificada es mucho más cara para Z3 que la fragmento lineal (que siempre es rápido y
decidible) — puede no terminar o devolver `Unknown`. La generalización de `:*:` es correcta y
deseada, pero cualquier trabajo futuro sobre el encoding a SBV (`sAExp`) debería tener esto en
mente si aparecen timeouts con programas que usan productos de variables no constantes.

**Estado de la normalización** (implementado en esta sesión): `normArit`/`simplifyArit` en
`Imp.hs` ya no usan `weightVar` (que sólo sabía tratar el caso lineal y daba resultados
silenciosamente incorrectos ante un producto no lineal). Ahora usan una forma polinomial
canónica explícita:

- `Monomial = Map Name Int` (variable → exponente; `Map.empty` = monomio constante).
- `Poly = Map Monomial Constant` (coeficiente por monomio; invariante: nunca guarda 0).
- `toPoly`/`fromPoly` expanden sumas/productos (`mulPoly` distribuye) y reconstruyen un
  `AExp` canónico, ordenado por grado y luego alfabéticamente.
- `normArit = fromPoly . toPoly`; `simplifyArit` quedó como una limpieza local barata
  (0/1 como neutros) que no expande productos de sumas, distinta de la normalización completa.
- Se agregó `instance Num AExp` (necesaria porque literales como `2 :*: Var "x"` ahora
  necesitan resolver `2` vía `fromInteger :: AExp`). **Cuidado**: `negate`/`(-1)` deben
  construirse con `Lit (-1) :*: e`, nunca con la sintaxis `(-1) :*: e` dentro de la propia
  instancia — causa recursión infinita en `negate` (bug real que ya se dio y se corrigió).
- Diseño elegido para el caso multi-término: **no** se muestra un coeficiente `1` explícito
  en cada sumando (ej. `x + y`, no `1*x + 1*y`) — más limpio, y es decisión del usuario del
  proyecto, no algo impuesto por los tests viejos.
- `Show AExp` (`showFactor`) también se corrigió: ahora ambos operandos de `:*:` se
  parentizan salvo que sean atómicos (`Lit`/`Var`). Antes sólo el operando derecho se
  parentizaba correctamente (heredado de cuando `:*:` era `Constant :*: AExp`, el izquierdo
  siempre era un literal); con `:*:` genérico, imprimir `(x + y) :*: z` daba `"x + y*z"`
  (ambiguo/incorrecto) — ahora da `"(x + y)*z"`.

**Sitios que dependían del tipo viejo `Constant :*: AExp`** (ya parcheados para compilar con
`AExp :*: AExp`, envolviendo la constante en `Lit`): `ImpParser.hs` (parser de `aexp`),
`ImpVCGen.hs` (`runTimeToArit`/`runTimeToArit'`), `ImpSBV.hs` (`sAExp`), y `Imp.hs`
(`simplifyRunTime`, `(-:)`).

## RunTime: de ponderación por constante a multiplicación entre RunTimes

Mismo espíritu que el cambio de `AExp`, aplicado a `RunTime`: el constructor `Constant :**:
RunTime` (ponderación por una constante, ej. `p :**: runt` para la probabilidad de una rama de
un `pif`) se reemplazó por `RunTime :**: RunTime`, un producto genuino entre dos RunTimes. Un
peso constante ahora se escribe `RunTimeArit (Lit k) :**: runt` en vez de `k :**: runt` con `k
:: Constant`.

- Se agregó `instance Num RunTime` (mismo motivo que `instance Num AExp`): permite seguir
  escribiendo literales enteros directo como RunTime (`2 :**: rtVar "x"`, `0 :**: r`, `1 :**:
  r`), resolviendo el literal vía `fromInteger`. Gracias a esto **ningún sitio de test tuvo
  que tocarse** — `2 :**: rtVar "x"` sigue tipando igual que antes, sólo que ahora `2` se
  resuelve a `RunTime` en vez de a `Constant`.
- Mismo cuidado que en AExp: `negate`/`(-1)` en la instancia se construyen directo con
  `RunTimeArit (Lit (-1)) :**: runt`, nunca con `(-1) :**: runt` dentro de la propia
  instancia (recursión infinita). `(--:)` (azúcar sintáctica de la resta de RunTime) se
  reescribió con esa misma construcción directa, sin depender de la instancia `Num`.
- `simplifyRunTime`/`deepSimplifyRunTime` (`Imp.hs`) se generalizaron: las reglas de
  cero/uno/fusión de pesos anidados ahora valen para ambos lados de `:**:` (antes sólo el
  lado izquierdo podía ser una constante, así que sólo hacía falta chequear ese lado); se
  agregó la regla `RunTimeArit arit_1 :**: RunTimeArit arit_2 = RunTimeArit
  (completeNormArit (arit_1 :*: arit_2))`, que generaliza la vieja "constante * RunTimeArit"
  a producto real entre dos expresiones aritméticas (vía la normalización polinomial de
  `AExp` ya existente). `sustRunTime`/`freeVarsRunTime`/`Show RunTime` (`showRTFactor`, análogo
  a `showFactor` de AExp) también se generalizaron a ambos lados de `:**:`.
- `ImpVCGen.hs`: todos los sitios que ponderaban por una probabilidad (`vcGenerator`,
  `vcGenerator'`, `cfPWhile`, para `PIf`/`PWhile`) ahora envuelven la constante con `rtLit`
  antes de usar `:**:`. `runTimeToArit`/`runTimeToArit'`/`evalCondition`/`getBExp` se
  generalizaron para recorrer ambos operandos de `:**:` (antes sólo recorrían el RunTime del
  lado derecho, ya que el izquierdo era una `Constant` sin BExp/AExp que recorrer).
- `ImpParser.hs`: el parser de `runtime` sigue aceptando la misma sintaxis concreta
  (`rational ** runtimeBase`), pero ahora construye `RunTimeArit (Lit rational) :**:
  runtimeBase` en vez de `rational :**: runtimeBase` — no se agregó sintaxis nueva para
  multiplicar dos RunTimes arbitrarios (igual que `aexp` tampoco expone `aexp * aexp` en la
  gramática concreta), sólo se generalizó la representación interna. **Nota: esto quedó
  superado en la sesión siguiente** — ver "RunTime: indicatriz como constructor propio
  (RunTimeBExp)" más abajo, donde `**` sí pasa a ser un operador infijo genérico
  `runtime ** runtime` en la gramática concreta.

## RunTime: indicatriz como constructor propio (RunTimeBExp)

Sesión posterior a la de arriba. Hasta acá, la indicatriz de `RunTime` era un constructor
dedicado de dos campos, `BExp :<>: RunTime` (una condición ponderada por un RunTime — el `p2_1`
`[x>0]<>x` de `ImpProgram.hs` valía 0 si `x>0` era falso, o `x` si era verdadero). Esta sesión
lo reemplazó por `RunTimeBExp BExp`, un constructor de un solo campo que **es** la indicatriz
(vale 0 o 1) y nada más — para ponderarla por algo se usa la multiplicación genérica `:**:` que
ya existía (`RunTimeBExp e_b :**: runt` en vez de `e_b :<>: runt`). El motivo: antes de este
cambio la indicatriz vivía fuera del sistema de multiplicación de `RunTime` (una excepción con
su propio constructor), lo cual ya no tenía sentido justo después de haber generalizado `:**:`
a multiplicación genuina entre dos `RunTime` cualesquiera — ahora la indicatriz es un factor
más, no un caso aparte.

Decisiones de diseño tomadas (todas confirmadas explícitamente antes de implementarlas, no
asumidas):

- **`Show`/parser**: se retiró la sintaxis concreta dedicada `[b] <> algo`. Una indicatriz
  ponderada ahora se imprime y se parsea con la multiplicación genérica: `[b]**algo`. `Show
  RunTime` perdió sus casos especiales para `:<>:` — `RunTimeBExp e_b` imprime sólo `"[e_b]"`
  (átomo, como `RunTimeArit`), y cualquier ponderación cae en el caso genérico de `:**:`
  (`showRTFactor` trata `RunTimeBExp _` como atómico, sin paréntesis, igual que `RunTimeArit`).
- **Gramática de `**`**: como consecuencia de retirar `<>`, hacía falta alguna forma de escribir
  "indicatriz por algo" en sintaxis concreta. Se generalizó `**` de "`rational ** runtimeBase`"
  (un literal a la izquierda nada más) a un operador infijo genérico en la tabla de
  `buildExpressionParser` — `runtime ** runtime`, con más precedencia que `++`/`--` (fila
  propia antes en la tabla) — así que `[b]**y`, `2**y` y `[b]**2` se parsean con la misma
  regla. `indArit`/`indicator <* "<>"` se eliminaron del parser; `runtimeBase` quedó como
  `indicator <|> aritRunTime`, y `term` de `runtime` como `try runtimeBase <|> parens runtime`
  (el `parens runtime` cubre el caso en que el factor de la derecha no es un `aexp` puro, ej.
  `[b] ** (w ++ 1)`, que `aexp` no puede parsear por sí solo).
- **`(<>:) :: RunTime -> AExp -> RunTime`**: antes exigía que el lado izquierdo fuera
  literalmente una indicatriz "pura" (peso `RunTimeArit (Lit 1)`) y fallaba con `error` en
  cualquier otro caso — un chequeo defensivo pensado para el sitio del parser que ya no existe.
  Primero se generalizó a sugar puro sin restricción (`runt <>: arit = runt :**: RunTimeArit
  arit`), pero al quedar redundante con `:**:` + `RunTimeArit` directo (y sin ningún sitio que
  lo siguiera usando) el usuario pidió eliminarlo directamente — ya no existe en `Imp.hs`.
- **Canonicalización de `:**:` — `flattenMul`/`buildMul`, no reasociación por pares**: el primer
  intento fue agregar una regla de reasociación simétrica para `:**:` (mirando el precedente de
  la reasociación que ya existía para `:++:`, sólo hacia la derecha). Se abandonó **por una
  razón concreta, no de estilo**: combinar reasociación hacia la derecha (`r1**(r2**r3)`, ya
  existente) con una regla simétrica hacia la izquierda (`(r1**r2)**r3`, la nueva) entra en
  loop infinito cuando el par que toca fusionar primero no reduce a nada — una regla lo empuja
  hacia un lado, la otra lo empuja de vuelta, indefinidamente. Se verificó a mano que esto
  realmente cuelga antes de descartarlo. `:++:` no tiene este problema porque sólo reasocia en
  una dirección. La solución adoptada — pedida explícitamente así por el usuario, para separar
  con claridad la parte booleana de la aritmética de cualquier cadena de productos — es aplanar
  toda la cadena de una vez (`flattenMul :: RunTime -> [RunTime]`, recursivo sobre ambos lados
  de `:**:`) y reconstruirla en una única pasada (`buildMul :: [RunTime] -> RunTime`): junta
  todas las indicatrices (`RunTimeBExp`) de la lista en una única conjunción a la izquierda
  (vía `simplifyBExp`), todos los pesos aritméticos (`RunTimeArit`) en un único polinomio a la
  derecha (vía `completeNormArit`), deja cualquier otro factor (ej. una suma `:++:` usada como
  peso) tal cual multiplicando al final, resuelve los casos absorbentes (`RunTimeBExp False'` o
  un polinomio que da `Lit 0` → `rtZero`) y omite cualquier parte que colapse al neutro
  (`RunTimeBExp True'`/`Lit 1`; si absolutamente todo colapsa, el resultado es `rtOne`). Esto
  reemplaza (sin riesgo de no-terminación, y generalizando a cualquier profundidad de
  anidamiento, no sólo la adyacente) el bloque entero de reglas sueltas que tenía
  `simplifyRunTime` para `:**:` (ceros/unos/`RunTimeBExp` True'-False'/fusión
  `RunTimeArit*RunTimeArit`/fusión `RunTimeBExp*RunTimeBExp`/reasociación) — la vieja regla
  "`b1 :<>: (b2 :<>: r) = (b1:&:b2):<>:r`" queda subsumida acá sin necesitar caso dedicado.
- **`asIndicatorWeight` — reconocer una indicatriz con o sin peso explícito**: las reglas 1/1b
  de `simplifyRunTime` (fusionar `[b]**r1 ++ [b]**r2` en `[b]**(r1++r2)`, y `[b]**r ++
  [!b]**r` en `r`) originalmente sólo reconocían la forma explícita `RunTimeBExp e_b :**: r`.
  Bug real encontrado al testear: `buildMul` limpia el peso redundante `**1` de cualquier
  indicatriz (`[b]**1` → `[b]` desnudo), así que una indicatriz que ya pasó por `buildMul` con
  peso 1 deja de tener la forma `_ :**: _` y las reglas 1/1b dejaban de reconocerla, rompiendo
  la fusión. Se agregó `asIndicatorWeight :: RunTime -> Maybe (BExp, RunTime)` (`RunTimeBExp b
  :**: r` da `(b, r)`; `RunTimeBExp b` desnuda da `(b, rtOne)`; cualquier otra cosa da
  `Nothing`) y las reglas 1/1b se reescribieron con pattern guards (`Just (b1, r_1) <-
  asIndicatorWeight t_1`) en vez de pattern-match directo, para que reconozcan una indicatriz
  sin importar si ya perdió su `**1` explícito.
- `sustRunTime`/`freeVarsRunTime`/`getBExp`/`evalCondition` (`ImpVCGen.hs`)/`vcGenerator`/
  `vcGenerator'`/`cfWhile` (`ImpVCGen.hs`) se adaptaron agregando un caso hoja para
  `RunTimeBExp` y reusando la recursión genérica que ya existía para `:**:` — no fue necesario
  ningún caso combinado "indicatriz seguida de peso" en la mayoría, porque una vez que
  `RunTimeBExp b` es una hoja, `f (r_1 :**: r_2) = f r_1 ++ f r_2` (ya existente) reproduce sola
  el viejo comportamiento. La excepción es `evalCondition`, donde sí se agregó un caso explícito
  para `RunTimeBExp bexp2 :**: runt` (en vez de dejar que la recursión genérica lo partiera en
  dos llamadas independientes) para reproducir exactamente el comportamiento viejo: cuando la
  condición evaluada coincide, el resultado es directo `evalCondition bexp1 runt` (la indicatriz
  desaparece, sin dejar un `1 :**: ...` pendiente de simplificar), y cuando es su complemento
  colapsa a `rtZero` de una, sin depender de que algo más simplifique después. `getBExp`
  preserva el detalle de que, si la condición es una negación (`RunTimeBExp (Not bexp)`), se
  guarda el `bexp` sin negar (igual que antes), para que `allContext` siga trabajando con la
  forma "positiva" de cada condición.
- Todos los strings de programas de prueba en `ImpProgram.hs` que usaban `<>` (`p2_1`,
  `cdkcMenos`, `cdkcMas`, `p2_2`, `cdvcMas`, `cdvcMenos`, `p4_7`, `p4_10`, `p4_11`, `p4_13`,
  `p4_14`, `p4_15`, y los `ind_*` de ejemplo) se migraron a `**`. De paso, `ind_3` (marcado
  `--falla` porque el viejo `<>` sólo aceptaba un `aexp` como peso, y su peso era el runtime
  compuesto `w ++ 1`) ahora sí parsea, porque `**` genérico no tiene esa restricción — se dejó
  el comentario actualizado explicando por qué en vez de borrarlo.

Verificado: `189 examples, 0 failures, 5 pending` con `cabal test runtime-test` y con
`stack test` (mismo conteo que antes de esta sesión más los tests nuevos que se agregaron/
reescribieron para `RunTimeBExp` en `ImpSpec.hs`/`ImpParserSpec.hs`).

## getExistencialAndUniversalVars / SolverInput' (ImpVCGen.hs)

Cambios posteriores al refactor de `RunTime`, en la parte de `ImpVCGen.hs` que clasifica
variables y arma el input para SBV:

- `getExistencialAndUniversalVars` clasifica un nombre como **universal** si aparece asignado
  (`Set`/`PSet`) o en una guarda (`If`/`While`), y como **existencial** si aparece libre en
  algún invariante (`freeVarsRunTime inv`) pero nunca como universal — es puramente sintáctico,
  no distingue "rol" de la variable más que por dónde aparece su nombre. `PSet x parit` antes
  sólo aportaba `x` a universales, ignorando las variables libres de `parit` (ej. `x :~ <y>`
  perdía a `y`); se corrigió agregando `freeVarsPAExp :: PAExp -> Names` en `Imp.hs` y usándola
  en `get_variables (PSet x parit)`.
- La lista de universales (`universal_variables`, a diferencia de `exist_variables`) **no** pasa
  por `rmdups` — puede traer nombres repetidos (ej. la misma variable aparece en una guarda y
  también asignada en el cuerpo del loop). No se tocó esa función; en su lugar, `SolverInput'`
  (`existential`/`for_all`) cambió de `Names` (`[Name]`) a `Set Name` (`Data.Set`), así que el
  duplicado se absorbe justo al construir el input para SBV. `ImpSBV.makeSBVModel'` convierte
  con `Set.toList` antes de pasarle los nombres a `sReals`/`mkUniversales` (que siguen tomando
  listas).
- `programToSolverInput :: Program -> SolverInput'` (preexistente) junta **todas** las
  restricciones del programa (una por cada `while`/`pwhile`) en un solo problema de SBV —
  `existential`/`for_all` salen de clasificar **todo** el programa, no de las restricciones, así
  que un programa sin loops igual reporta sus variables ahí aunque `solver_formulaes` quede
  vacío.
- `programToSolverInputs :: Program -> [SolverInput']` (nueva) da un `SolverInput'` **por cada**
  restricción/invariante en vez de uno solo combinado — pensada para poder resolver/reportar
  cada invariante por separado ("para el invariante 1 se generan estas restricciones, para el
  invariante 2 estas otras"). No etiqueta a qué `while` corresponde cada elemento más allá del
  orden de la lista (mismo orden en que `vcGenerator` recorre el programa) — alcanza mientras
  el usuario use nombres de template distintos entre sí y de las variables del programa. Cada
  `SolverInput'` individual filtra su `existential`/`for_all` a las variables que realmente
  aparecen en sus propias fórmulas vía `relevantVars` (`Set.intersection`), en vez de reusar la
  lista global completa — aunque en loops anidados con dependencia real entre ellos (la
  continuación de un `while` interno arrastra el contexto del externo) el filtro igual puede
  terminar necesitando las variables de ambos, lo cual es correcto y no un bug del filtro.
- Tests: `ImpVCGenSpec.hs` cubre las cuatro funciones de arriba con programas construidos a mano
  (`Set`/`PSet`/`If`/`While` anidado), incluyendo la regresión del fix de `PSet` y el caso de
  dos `while` anidados con templates de nombres distintos.

## mkUniversales: de tope de 3 a cantidad arbitraria (ImpSBV.hs)

`mkUniversales` cuantifica universalmente las variables de `for_all` de un `SolverInput'` (vía
`quantifiedBool`/`ForallN` de SBV). Hasta esta sesión tenía un **tope duro de 3**: hacía `case
length names of 0/1/2/3 -> ...`, con un `ForallN k "u" AlgReal` literal por cada aridad, y
`error "Máximo 3 variables universales soportadas"` para 4 o más.

Ese tope **no era un límite real de Z3 ni de la teoría** (aritmética real cuantificada sigue
siendo cara para cualquier cantidad de variables, eso no cambió) — era un artefacto de que
`ForallN n nm a` necesita `n` como Nat de **tipo** (`DataKinds`), resuelto en compilación vía la
typeclass `Skolemize`, así que sólo se habían escrito 4 casos a mano (0 a 3). Además, en la
versión de SBV que usa este proyecto (14.3) ya no existen los `forall`/`exists` monádicos
clásicos (los que permitían declarar variables cuantificadas una por una dentro de `Symbolic`)
— sólo queda la API tipada `Forall`/`ForallN`/`quantifiedBool`.

**Fix**: `GHC.TypeNats.someNatVal :: Natural -> SomeNat` empaqueta un Nat existencial a partir
de un número que sólo se conoce en runtime (`length names`); al desempaquetarlo con
`SomeNat (_ :: Proxy k)`, `k` queda disponible como el mismo tipo de Nat que pide `ForallN`, sin
importar cuánto valga — no hace falta ningún pragma nuevo (`DataKinds`/`ScopedTypeVariables`
/`TypeAbstractions`, que el archivo ya tenía, alcanzan). Se verificó a mano contra Z3 antes de
aplicar el cambio (con 0 a 8 variables universales, casos sat/unsat esperados y correctos en
ambos) y después con `mkUniversales` real del proyecto (5 variables, donde antes tiraba el
`error`, y el caso borde de 0 variables).

Los imports `Data.SBV.Dynamic`/`Data.SBV.Internals` con el comentario `-- svMkSymVar, KReal,
VarContext(..), Quantifier(..)` / `-- symbolicEnv` que ya estaban en `ImpSBV.hs` sugerían una
posible vía alternativa (construir las variables cuantificadas a mano con la API dinámica/no
tipada de SBV) — se investigó pero **no se usó**: `symbolicEnv` ni siquiera existe con ese
nombre en la versión actual de SBV, y reproducir a mano la lógica de Skolemización que hace
`ForallN`/`Skolemize` internamente es mucho más riesgoso (fácil de dejar mal la semántica de
cuantificadores alternados) que la solución con `someNatVal`, que reusa toda la maquinaria
tipada existente y sólo cambia de dónde sale el `n`.

Tests: `test/ImpSBVSpec.hs` (nuevo — antes no existía ningún spec para `ImpSBV.hs`) cubre
`mkUniversales`. A diferencia del resto de la suite, estos tests sí invocan a Z3 de verdad
(`isSatisfiable`) — un `SBool` armado por `mkUniversales` es opaco, no hay forma de verificar
qué cuantificador construyó sin resolverlo. Casos cubiertos: 0 variables (caso borde, antes su
propia rama), una sola variable universal fuerza que la fórmula valga para *todo* x (si se
tratara como existencial daría sat), regresión de más de 3 variables (5, donde antes tiraba el
`error`), que cada nombre reciba una variable distinta (no se alíen entre sí, chequeado con
"∀x,y. x==y" que debe salir `Unsatisfiable`), la combinación típica ∃∀ que usa
`programToSolverInput`/`makeSBVModel'`, y que el `Map` que recibe la función callback tenga
exactamente los nombres pedidos. Las fórmulas se mantienen chicas (aritmética lineal, pocas
variables) para que sigan siendo rápidos (~0.3s el total de la suite).

## Flujo de impresión, modo nuevo (ImpIO.hs: completeRoutine' / app/Main.hs: run')

El modo antiguo (`completeRoutine`/`routineInput`/`restrictionsToSolver`/`showRestrictions`)
trata todas las variables como libres y prueba, POR CADA CONTEXTO por separado, si negar la
restricción es satisfacible ("proceder por contradicción" — el propio código lo marca como poco
claro, ver el comentario sobre `restrictionsToSolver` en `ImpVCGen.hs`). Se agregó un flujo
paralelo — `completeRoutine'` (`ImpIO.hs`) / `run'` (`app/Main.hs`) — que usa
`programToSolverInputs`: un único problema de SBV por obligación (por `while`/`pwhile`), con
cuantificación ∃∀ real vía `mkUniversales` (∃ variables de template, tal que ∀ variables de
programa, la obligación completa —todos los contextos a la vez, no uno por uno— se cumple). El
formato de salida sigue la misma estructura que `completeRoutine` (separadores de `-`,
"Obligación de prueba [n]", mensajes finales) para que sea comparable a simple vista.

**La polaridad de "válida" se invierte según si la obligación tiene variables existenciales**
(`showSolverInput'` en `ImpIO.hs` maneja ambos casos explícitamente):
- **Sin existenciales** (invariante ya concreto, el caso típico de `ImpProgram.hs`): se prueba
  por contradicción igual que el modo antiguo — `Unsatisfiable` = no hay contraejemplo = válida;
  `Satisfiable` = el modelo ES el contraejemplo (se imprime con los nombres de `for_all`) = no
  válida.
- **Con existenciales** (invariante-plantilla, ej. `inv = a**x`): se le pide a Z3 un testigo sin
  negar — `Satisfiable` = SÍ existe un testigo = válida (se imprime con los nombres de
  `existential`, "Testigo encontrado..."); `Unsatisfiable` = ningún testigo funciona = no válida
  (no hay "contraejemplo" que mostrar, sólo el mensaje de que la forma del template no alcanza).

**Verificado antes de darlo por terminado** (con `while(x > 0){inv = 1 ++ 2**[x>0]<>x}{x:= x-1}`,
`p2_1` del banco de `ImpProgram.hs`): `completeRoutine'` y `completeRoutine` dan **exactamente el
mismo veredicto y el mismo contraejemplo** (`No es válida`, `x = 0.5 Real`) — el modo nuevo no
cambia ninguna semántica de validez, sólo cómo se arma y resuelve el problema de SBV. Se probaron
además a mano los otros tres casos: concreto-sin-existentes-válido, template-con-testigo (ej.
`while(false){inv = a}{skip}` → `a = 1.0`) y template-sin-testigo (ej. `while(x>0){inv =
a**x}{x:=x-1}`, construido a mano porque `a**x` — variable por variable — no es sintaxis
concreta soportada, ver sección de RunTime más arriba).

**Hallazgo de paso, no introducido ni arreglado acá**: al probar el caso "válido sin
existenciales" con `while(false){inv = [x >= 0]<>x}{skip}`, `x` termina clasificada como
**existencial** por `getExistencialAndUniversalVars` (nunca aparece asignada ni en ninguna
guarda real del programa — la guarda es la constante `false`), aunque semánticamente el usuario
probablemente la pensó como variable de programa/estado. El resultado sale "válida" con testigo
`x = 1.0`, que es engañoso si `x` debía cuantificarse ∀ y no ∃. Es la misma heurística puramente
sintáctica ya documentada en la sección "getExistencialAndUniversalVars / SolverInput'" más
arriba — sigue valiendo el supuesto de que el usuario nombra sus templates distinto a las
variables reales del programa, y acá se rompe sólo porque el programa de prueba nunca usa `x`
en ningún otro lado. No se tocó nada al respecto — queda como un `TODO` fechado (2026-09-03)
justo arriba de `getExistencialAndUniversalVars` en `ImpVCGen.hs`, con el diagnóstico completo
(la condición y el cuerpo del while YA se usan para clasificar; el gap es específicamente el
caso donde ninguno de los dos menciona la variable).

**Tests** (`test/ImpIOSpec.hs`, nuevo — antes `ImpIO.hs` no tenía ningún test, sólo se había
verificado a mano con scripts sueltos): cubre `showModel'`/`showSolverInput'`/`showSolverInputs'`/
`completeRoutine'` a través de las 4 combinaciones de polaridad (concreto válido/inválido,
template con/sin testigo) más los casos "sin ciclos" y "varias obligaciones, alguna inválida".
Como estas funciones imprimen por stdout, los tests usan `capture` del paquete `silently`
(agregado como dependencia de test en `package.yaml`/`runtime.cabal`) para redirigir y devolver
lo impreso como `String`, en vez de reimplementar a mano la duplicación de `Handle`s. Los asserts
verifican los mensajes/estructura del output (qué rama se imprimió), **no** valores numéricos
exactos de contraejemplos/testigos — cuál `AlgReal` concreto elige Z3 entre varios igual de
válidos no está garantizado que sea siempre el mismo entre corridas.

**Ejecutable nuevo para correr el banco a mano: `informe-examples`**. A pedido del usuario, se
agregó `app-informe/InformeExamples.hs` (registrado como ejecutable propio en `package.yaml`,
`main: InformeExamples.hs`, `source-dirs: app-informe`) que corre `completeRoutine'` sobre ~30
programas del banco de `ImpProgram.hs` (los mismos que cubre `test/ImpProgramSpec.hs`, más
`p4_7`), cada uno con un encabezado que cita la página/figura del informe correspondiente
cuando aplica. Se ejecuta con `cabal run informe-examples` o (tras `stack build`) `stack exec
informe-examples`. **Nota de layout**: el archivo NO se puso en `app/` junto a `Main.hs` —
ambos declaran `module Main`, y si comparten `source-dirs` hpack termina agregando
`InformeExamples.hs` como `other-modules` del target `runtime-exe` también (autodiscovery),
lo que rompe la compilación de `runtime-exe` con "File name does not match module name: Saw
Main, Expected InformeExamples". Se resolvió dándole su propio `source-dirs: app-informe`, sin
tocar el layout de `runtime-exe`. Como es un ejecutable nuevo (no una dependencia), no hizo
falta `cabal update` esta vez — sólo `stack build`/`cabal build` para regenerar y confirmar.

## Tests con el banco de la memoria (test/ImpProgramSpec.hs)

Módulo de test nuevo, separado de `ImpVCGenSpec.hs`/`ImpSBVSpec.hs`: corre, con el **modo
nuevo** (`programToSolverInputs`/`mkUniversales`, ver secciones de arriba), los programas del
banco de `ImpProgram.hs` que corresponden a las categorías de la memoria con **invariante
concreto** (sin variables de template/existenciales) — 25 tests, ~0.9s en total (invocan a Z3
de verdad, una vez por cada `while`/`pwhile` de cada programa).

Se excluyen a propósito (usan templates, fuera del alcance de este spec): `p4_7` (usa `A` como
invariante-plantilla explícito) y `cdvcMenos` **sólo como programa "sin templates" normal** —
sigue testeado, pero en su propio `describe` (ver abajo), porque la heurística de
`getExistencialAndUniversalVars` termina clasificando su `x` como existencial de todas formas.

**Varios veredictos se verificaron citando el cálculo manual del propio informe** (`dummy.pdf`,
ver **Contexto extenso**), no sólo corriendo la herramienta y aceptando el resultado:
- `Cdkc-`/`cdkcMenos` (informe pp. 44-46): el modo nuevo reproduce **exactamente** el
  contraejemplo manual, `x = 1/2`.
- `Cdkc+`/`cdkcMas` (p. 68), `Cdvs`/`cdvs` (pp. 66-67), `Cdvc+`/`cdvcMas` (pp. 71-72),
  `Cpvs`/`cpvs` (pp. 76-77): coinciden con el veredicto manual del informe.
- `Cpkc-`/`cpkcMenos` (informe pp. 78-79): el modo nuevo reproduce **exactamente** las 3
  obligaciones manuales (`9/2<=3` inválida ×2, `3<=3` válida).
- `Cpvc` (informe pp. 80-82): `cpvcMenos`/`cpvcMas` en `ImpProgram.hs` corresponden a las dos
  aproximaciones del invariante `I₂` que discute el informe (`I₂^min` vs. otra aproximación del
  denominador) — el modo nuevo confirma que la primera deja el `while` interno no válido y la
  segunda no.
- La familia `p4_6`/`p4_8`/`p4_9` (`while(c==1){inv=1+K*[c==1]}`, K=4/3/5) es una linda
  demostración de que el modo nuevo entiende la noción de cota ajustada vs. floja: K=4 es la
  cota exacta (válida), K=3 la subestima (no válida), K=5 la sobreestima pero sigue siendo una
  cota válida (más floja).

**Hallazgo confirmado con el informe, no arreglado acá** (`describe` dedicado al final del
archivo): para `Cdvc-`/`cdvcMenos` (`while(false){inv=[x>=0]<>x}{skip}`), el informe prueba **a
mano** que el invariante NO es válido (contraejemplos `x=-1` y `x=0`, pp. 73-74) — es
exactamente el mismo programa, carácter por carácter. El modo nuevo, en cambio, clasifica `x`
como **existencial** (nunca se asigna ni aparece en ninguna guarda real; la guarda del `while`
es la constante `false`) y por eso responde "válido" con testigo `x=1`. No es un bug del
solver ni de `mkUniversales` — es la limitación ya documentada de `getExistencialAndUniversalVars`
(heurística puramente sintáctica) tropezando con un caso real y no contrivado, tomado
directamente de la validación oficial de la memoria. El test lo deja explícito en vez de
esconderlo: confirma que `x` quedó existencial y que el veredicto resultante (`True`) es el
opuesto al que demuestra el informe.

## Deuda técnica pre-existente y documentada (no introducida por este refactor)

El propio informe (§6.2 "Discusión de los resultados") señala que **`RunTime` no tiene forma
normal**: la simplificación es componente a componente (`f1 <= f2` se simplifica simplificando
`f1` y `f2` por separado, nunca comparándolos entre sí), y `==` sobre `RunTime` no es
semántico (`x - 1` y `-1 + x` son estructuras distintas). Consecuencias conocidas:
obligaciones de la forma `f <= f` no colapsan a `0 <= 0`; expresiones booleanas compuestas
sin normalizar (`[a]·[b]·[c] <= 0`) generan `2^n` problemas lineales en vez de 1. El informe
propone esto también como trabajo futuro (§6.4 "Definir forma normal de los tiempos de
ejecución RunTime"). **Sigue sin resolverse** — no es parte del cambio de `AExp`, pero si se
sigue trabajando en `simplifyRunTime`/`deepSimplifyRunTime`, conviene saber que el problema es
real y ya está diagnosticado, no hay que redescubrirlo.

### Ya resuelto en esta sesión (quedaba documentado acá como pendiente, ya no lo es)

`ImpVCGen.hs` tenía errores de tipo **no relacionados** con el cambio de `AExp`, que
impedían un build completo de la librería. Ya están corregidos:

- `vcGenerator'` (casos `While`/`PWhile`): faltaban paréntesis — `ProgramVCGenInformation ... :
  restrictionsInformation vc_p` se leía como *"consar la lista de restricciones al final de
  una lista de `ProgramVCGenInformation`"* en vez de *"consar la restricción nueva al campo
  `restrictionsInformation :: [RestrictionInformation]`"*. Se corrigió agrupando el `:` dentro
  del segundo argumento de `ProgramVCGenInformation`.
- Los mismos dos casos usaban `fst vc_p` (resabio de cuando `vcGenerator'` devolvía una tupla
  `(RunTime, [RRunTime])`) en vez de `runtime vc_p`, el accessor del record
  `ProgramVCGenInformation` actual.
- `programToSolverInput` pasaba `rest :: [RRunTime]` directo a `restrictionsToImplications ::
  RRunTime -> [Implication]` — hacía falta `concatMap`.
- Al compilar `ImpVCGen.hs`, su campo de record `runtime` (de `ProgramVCGenInformation`) quedó
  expuesto y choca de nombre con el parser `ImpParser.runtime`; se resolvió calificando/
  ocultando el import según el archivo (`app/Main.hs`, `test/ImpVCGenSpec.hs`).
- `onlyInFirst` estaba declarada dos veces (líneas 108 y 337) en el archivo — se eliminó el
  duplicado.

## Tests

`runtime/test/`: `ImpParserSpec.hs` y `ImpSpec.hs` son specs de Hspec reales, con cobertura
completa de sus áreas (parser y `completeNormArit`/`aexpE`/`expectedValue`). `ImpVCGenSpec.hs`
tiene sus 5 tests originales de `restrictionsToImplications` todavía en `pending` (con TODOs
explícitos, esqueleto sin terminar) — **pero** desde esta sesión también cubre con tests reales
la parte de armado de `SolverInput'` (`getExistencialAndUniversalVars`, `relevantVars`,
`programToSolverInput`, `programToSolverInputs`), usando programas construidos a mano (AST
directo, sin pasar por el parser) como `progAssign`/`progPSet`/`progIf`/`progLoop`/
`progNested` (este último con dos `while` anidados, templates `a`/`c` de nombres distintos —
el mismo escenario documentado en la sección de RunTime más arriba). Vale la pena notar dos
cosas no obvias que confirman esos tests:
- `programToSolverInput` (el singular) calcula `existential`/`for_all` a partir de **todo** el
  programa vía `getExistencialAndUniversalVars`, no de las restricciones — por eso un programa
  sin ningún `while` igual reporta sus variables en `for_all` aunque `solver_formulaes` quede
  vacío (`programToSolverInputs`, en cambio, sí queda en `[]` en ese caso, porque itera sobre
  `rest`).
- En `progNested`, **ambos** `SolverInput'` de `programToSolverInputs` terminan necesitando las
  dos variables de template (`a` y `c`) y las dos de programa (`x` e `y`), no sólo las del loop
  "propio" — es la dependencia real que ya se documentó en el comentario de
  `programToSolverInputs` (la continuación de un `while` anidado arrastra el contexto del
  `while` que lo contiene), no una falla del filtro `relevantVars`.

Estado: **189 examples, 0 failures, 5 pending** (correr `cabal test runtime-test` o
`stack test`, ver **Build**; el conteo subió de 187 a 189 en la sesión de "RunTime: indicatriz
como constructor propio (RunTimeBExp)", que agregó cobertura de parser para la indicatriz
ponderada). De paso se encontraron y corrigieron dos bugs reales en los tests mismos (no en el
código de producción):
- `ImpParserSpec.hs`, test "parsea for": el valor esperado no envolvía el cuerpo del `for` en
  `Seq Empty (...)`, aunque la gramática de `program` siempre envuelve así cualquier cuerpo
  parseado (el test de `while`, dos líneas arriba en el mismo archivo, sí lo hace bien —
  sirve de referencia).
- `ImpVCGenSpec.hs`: dos tests con `runtime = undefined`/`expected = undefined` (uno además
  referenciaba una variable `runtime` libre, nunca definida) quedaban como fallos con
  excepción no capturada en vez de marcarse `pending` como el resto de los stubs del archivo.

## Build

El proyecto compila y pasa los tests **tanto con `cabal` como con `stack`** — se verificaron
ambos en esta sesión (`stack` usa GHC 9.12.4, `cabal` usa GHC 9.10.3 vía ghcup; el código no
depende de rarezas de ninguno de los dos). `package.yaml` es la fuente de verdad (la usa
`stack` directo); `runtime.cabal` se genera desde ahí vía `hpack` y es lo que usa `cabal` — **si
tocás dependencias/build-tools, editá `package.yaml` y regenerá o replicá el cambio a mano en
`runtime.cabal`**, si no `stack build` te va a pisar el `.cabal` a la próxima.

```bash
cd runtime
cabal build && cabal test runtime-test    # o
stack build && stack test
```

`hie.yaml` apunta a un cradle de `stack` para HLS.

**Nota de esta sesión**: `hspec-discover` (el preprocesador que arma `test/Spec.hs`
descubriendo automáticamente los módulos `*Spec`) estaba declarado como `build-depends`
(dependencia de librería) en vez de `build-tools`/`build-tool-depends` (herramienta ejecutable
que hay que construir y exponer durante el preprocesado). Eso rompía `cabal test` con
`could not execute: hspec-discover`. Se corrigió agregando `build-tools:
hspec-discover:hspec-discover` en `package.yaml` (y su equivalente `build-tool-depends` en
`runtime.cabal`).

**Nota de otra sesión**: se agregó `silently` como dependencia del test-suite (sólo de tests,
no de la librería) para poder capturar stdout en `test/ImpIOSpec.hs` — igual que con
`hspec-discover`, se editó `package.yaml` y se dejó que `stack build` regenerara
`runtime.cabal`. Agregar esta dependencia nueva necesitó `cabal update` (bajar el índice de
paquetes de Hackage) porque el proyecto no traía un índice local que la conociera; de yapa,
eso hizo que `cabal` resolviera `sbv` a una versión más nueva (14.3→14.7) la próxima vez que
resolvió dependencias — no rompió nada (mismos 187 tests pasan), pero es la explicación si en
algún momento `cabal build`/`cabal test` usan una versión de `sbv` distinta a la que se ve en
`stack` (que mantiene la suya propia, fijada por el resolver de `stack.yaml`, sin verse
afectada por `cabal update`). Si hace falta reproducibilidad estricta de versión de `sbv` en
`cabal`, conviene fijarla explícita en `package.yaml` (`sbv == x.y.z`) en vez de dejarla sin
cota superior como está hoy.
