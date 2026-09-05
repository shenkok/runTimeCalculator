# runtime

Herramienta en Haskell que automatiza el cálculo del **tiempo de ejecución esperado** de
programas probabilísticos, usando la transformada `ert[·]` (Kaminski, Katoen, Matheja,
Olmedo — *Weakest Precondition Reasoning for Expected Run-Times of Probabilistic Programs*).
Además de calcular `ert[·]`, la herramienta genera las **obligaciones de prueba** asociadas a
los invariantes de los ciclos (`vcg[·]`, al estilo VCGen/lógica de Hoare) y las **verifica
automáticamente** con el SMT-solver Z3 vía la librería [SBV](https://hackage.haskell.org/package/sbv).

Este paquete (`runtime/`) es la continuación de una memoria de título ya finalizada — ver
`INFORME_TRABAJO_DE_T_TULO_LUIS_PINOCHET_GONZALEZ.pdf` y `material2020/Propuesta_Final_Luis_Pinochet.pdf`
en la raíz del repo — y está en pleno desarrollo (ver `CLAUDE.md` para el historial detallado
de las decisiones de diseño).

## Instalación

### Requisitos

- [GHC](https://www.haskell.org/ghc/) — probado con 9.10.3 (vía `ghcup`/`cabal`) y 9.12.4
  (vía `stack`).
- Uno de estos dos gestores de build (el proyecto funciona con cualquiera de los dos):
  - [Stack](https://docs.haskellstack.org/) — descarga su propio GHC automáticamente, es la
    forma más simple de empezar.
  - [Cabal](https://www.haskell.org/cabal/) (>= 3.x) + [ghcup](https://www.haskell.org/ghcup/).
- **[Z3](https://github.com/Z3Prover/z3)** instalado y disponible en el `PATH` — es el
  SMT-solver que usa `SBV` para verificar las obligaciones de prueba. Probado con Z3 4.8.12.
  - Debian/Ubuntu: `sudo apt install z3`
  - macOS: `brew install z3`
  - Verificar con `z3 --version`.

### Clonar y compilar

```bash
git clone <url-del-repo>
cd runTimeCalculator/runtime

# Con Stack
stack build

# — o — con Cabal
cabal build
```

`package.yaml` es la fuente de verdad de las dependencias/ejecutables/tests; `runtime.cabal`
se genera automáticamente desde ahí vía `hpack` (lo hace `stack build` solo; si usás sólo
`cabal`, regenéralo con `hpack` tras tocar `package.yaml`). Si nunca corriste `cabal update`
en esta máquina, puede hacer falta antes del primer `cabal build`.

### Correr los tests

```bash
stack test
# — o —
cabal test runtime-test
```

Estado actual: **189 examples, 0 failures, 5 pending** (los `pending` son esqueletos de tests
sin terminar en `ImpVCGenSpec.hs`, trabajo futuro — no bugs). Varios tests invocan a Z3 de
verdad, así que hace falta tenerlo instalado también para correr la suite completa.

### Ejecutar

```bash
# Modo interactivo (REPL de GHCi con las funciones run/run'/fp/fpp precargadas)
stack ghci
# — o —
cabal repl

# Ejecutable principal
stack exec runtime-exe
# — o —
cabal run runtime-exe

# Banco de ~30 ejemplos del informe, cada uno con su cita de página/figura
stack exec informe-examples
# — o —
cabal run informe-examples
```

## Uso interactivo

Desde `stack ghci` (carga `app/Main.hs`), las funciones principales son:

- **`run :: String -> IO ()`** — parsea un programa, calcula su `ert[·]`, genera las
  obligaciones de prueba de cada invariante y las manda a Z3 (modo antiguo: variables libres,
  un problema de SBV por contexto, prueba por contradicción).
- **`run' :: String -> IO ()`** — igual que `run`, pero con el modo nuevo de impresión
  (`completeRoutine'`): un problema de SBV por obligación, con cuantificación `∃∀` real vía
  `mkUniversales`.
- **`fp :: String -> String -> String -> String -> Int -> IO ()`** — calcula la iteración de
  punto fijo de orden `n` de la transformada de un `while` determinista (útil para *encontrar*
  candidatos a invariante a mano). Argumentos: runtime inicial, condición, cuerpo, runtime de
  continuación, número de iteraciones.
- **`fpp`** — análogo a `fp` para un `pwhile` probabilista (usa `PBExp` en vez de `BExp`).

Ejemplo:

```haskell
ghci> run "while(x > 0){inv = x}{x := x - 1}"
```

## Gramática

### Sintaxis abstracta (resumen — ver `src/Imp.hs` para la definición completa)

```haskell
data AExp                      -- Expresiones aritméticas (polinomiales)
  = Lit Constant                -- Literal numérico (racional)
  | Var Name                     -- Variable
  | AExp :+: AExp                -- Suma
  | AExp :*: AExp                -- Multiplicación

data BExp                      -- Expresiones booleanas
  = True' | False'
  | AExp :<=: AExp | AExp :==: AExp
  | BExp :|: BExp | BExp :&: BExp | Not BExp

data RunTime                   -- Tiempos de ejecución / cotas
  = RunTimeArit AExp             -- a partir de una expresión aritmética
  | RunTimeBExp BExp              -- indicatriz [b] (vale 0 o 1)
  | RunTime :++: RunTime          -- suma
  | RunTime :**: RunTime          -- multiplicación (pondera indicatrices, escala, etc.)

type PAExp  = Distribution AExp  -- Distribución discreta de expresiones aritméticas
newtype PBExp = Ber { p :: PConstant } -- Bernoulli(p)

data Program
  = Skip | Empty
  | Set Name AExp                       -- asignación
  | PSet Name PAExp                     -- asignación probabilista
  | Seq Program Program                 -- composición secuencial
  | If BExp Program Program             -- condicional
  | PIf PBExp Program Program           -- condicional probabilista
  | While BExp Program RunTime          -- ciclo, con invariante
  | PWhile PBExp Program RunTime        -- ciclo probabilista, con invariante
```

### Sintaxis concreta (resumen — ver `src/ImpParser.hs` para el parser completo)

```
aexp     ::= identifier | rational | rational * aexp | aexp + aexp | aexp - aexp | (aexp)

bexp     ::= true | false
           | aexp <= aexp | aexp == aexp | aexp >= aexp | aexp > aexp
           | aexp < aexp  | aexp != aexp
           | ! bexp | bexp && bexp | bexp || bexp | (bexp)

runtime  ::= aexp | [bexp]                -- runtime aritmético / indicatriz
           | runtime ** runtime            -- multiplicación (mayor precedencia)
           | runtime ++ runtime            -- suma
           | runtime -- runtime            -- resta (azúcar sintáctica)
           | (runtime)

paexp    ::= rational * <aexp> | paexp + paexp | (paexp)   -- distribución discreta
pbexp    ::= <rational>                                     -- Bernoulli(rational)

program  ::= skip | empty
           | identifier := aexp                             -- asignación
           | identifier :~ paexp                             -- asignación probabilista
           | if (bexp) {program} else {program}
           | it (bexp) {program}                             -- azúcar: if sin else
           | pif (<rational>) {program} pelse {program}
           | pit (<rational>) {program}                      -- azúcar: pif sin pelse
           | while (bexp) {inv = runtime} {program}
           | pwhile (<rational>) {pinv = runtime} {program}
           | for (integer) {program}                         -- azúcar: desenrolla el ciclo
           | program ; program
```

Notas:
- Los racionales se escriben como enteros (`3`) o como fracción `entero/entero` (`1/2`).
- `**` es un operador infijo genérico entre dos `runtime` cualesquiera (no sólo
  "constante `**` runtime"): `2 ** x`, `[b] ** y`, `[b] ** 2` y `x ** y` son todas válidas.
  No existe sintaxis concreta para multiplicar dos `aexp` arbitrarios (`aexp * aexp`); `aexp`
  sólo admite ponderación por un racional literal a la izquierda.

## Mapa de módulos (`src/`)

| Módulo | Rol |
|---|---|
| `Imp.hs` | Núcleo: ASTs de `AExp`, `BExp`, `RunTime`, `PAExp`/`PBExp` (probabilistas), `Program`; azúcar sintáctica; sustitución; `freeVars`; simplificación y normalización polinomial de `AExp`; simplificación de `BExp`/`RunTime`/`Program`; `expectedValue`/`aexpE` para calcular esperanzas sobre distribuciones. |
| `ImpParser.hs` | Parser (Parsec) de la sintaxis concreta descrita arriba, espejo de la sintaxis abstracta de `Imp.hs`. |
| `ImpVCGen.hs` | `vcg[·]`: recorre un `Program` y devuelve el runtime esperado más las obligaciones de prueba; clasifica variables existenciales/universales; arma el input final para SBV (`programToSolverInput`/`programToSolverInputs`). |
| `ImpSBV.hs` | Traduce `AExp`/`BExp`/implicaciones a `SBV`; `mkUniversales` cuantifica universalmente una cantidad arbitraria de variables. |
| `ImpIO.hs` | Formatea el output para consola: tiempo calculado, obligaciones de prueba, contraejemplos/testigos. Dos modos: `completeRoutine` (antiguo, variable libre, prueba por contradicción) y `completeRoutine'` (nuevo, cuantificación `∃∀` real). |
| `ImpProgram.hs` | Banco de programas de ejemplo/test en sintaxis abstracta, incluyendo las 12 categorías determinista/probabilista × ciclo/sin-ciclo × invariante correcto/incorrecto de la memoria de título. |

- **`app/Main.hs`** — entry point interactivo (`run`, `run'`, `fp`, `fpp`) y ejemplos sueltos.
- **`app-informe/InformeExamples.hs`** — ejecutable `informe-examples`, corre `completeRoutine'`
  sobre el banco de `ImpProgram.hs` citando la página/figura del informe correspondiente.
- **`test/`** — specs de Hspec (`ImpSpec.hs`, `ImpParserSpec.hs`, `ImpVCGenSpec.hs`,
  `ImpSBVSpec.hs`, `ImpIOSpec.hs`, `ImpProgramSpec.hs`); `Spec.hs` se genera automáticamente
  vía `hspec-discover`.

## Documentación adicional

- `CLAUDE.md` (raíz del repo) — historial detallado de las decisiones de diseño de cada
  refactor, deuda técnica conocida y notas de build.
- `INFORME_TRABAJO_DE_T_TULO_LUIS_PINOCHET_GONZALEZ.pdf` — memoria de título completa de la
  versión lineal de la herramienta: definición formal de `ert[·]`/`vcg[·]`, algoritmo de
  linealización, y banco de validación (12 subcategorías de programas de prueba).
- `material2020/Propuesta_Final_Luis_Pinochet.pdf` — propuesta de investigación original.

## Licencia

BSD3 — ver `LICENSE`.
