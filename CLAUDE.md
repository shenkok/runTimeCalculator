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
abajo). El código actual está en pleno refactor de una decisión de diseño puntual: generalizar
las expresiones aritméticas de *lineales* a *polinomiales*. Si vas a tocar `Imp.hs`, lee la
sección "AExp: de lineal a polinomial" antes de asumir cómo debería comportarse.

**Estado actual de la rama** (`feature/change_arit_expression`): el refactor de `AExp` está
terminado y el proyecto compila y pasa sus tests de punta a punta, tanto con `cabal` como con
`stack` (ver sección **Build**) — `89 examples, 0 failures, 5 pending`. Los 5 `pending` son
esqueletos de tests sin terminar en `ImpVCGenSpec.hs` (trabajo futuro, no bugs).

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
- **`ImpSBV.hs`** — traduce `AExp`/`BExp`/implicaciones a `SBV`; `mkUniversales` cuantifica
  variables universales (**tope duro de 3**, ver `error "Máximo 3 variables..."`) — esto es
  intencional: aritmética real cuantificada es cara, y el `while` para acotar cuenta como
  contradicción sobre variables existenciales.
- **`ImpIO.hs`** — formatea el output para consola (tiempo calculado, obligaciones,
  contraejemplos).
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
`mkUniversales` (tope de 3) sobre `AlgReal`. Aritmética real no lineal cuantificada es mucho
más cara para Z3 que la fragmento lineal (que siempre es rápido y decidible) — puede no
terminar o devolver `Unknown`. La generalización de `:*:` es correcta y deseada, pero
cualquier trabajo futuro sobre el encoding a SBV (`sAExp`) debería tener esto en mente si
aparecen timeouts con programas que usan productos de variables no constantes.

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
está mayormente sin implementar (5 tests en `pending`, con TODOs explícitos) — es un
esqueleto, no asumir que cubre `vcg[·]`.

Estado: **89 examples, 0 failures, 5 pending** (correr `cabal test runtime-test` o
`stack test`, ver **Build**). De paso se encontraron y corrigieron dos bugs reales en los
tests mismos (no en el código de producción):
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
