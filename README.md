# runTimeCalculator

Herramienta en Haskell que automatiza el cálculo del **tiempo de ejecución esperado** de
programas probabilísticos, usando la transformada `ert[·]` (Kaminski, Katoen, Matheja, Olmedo —
*Weakest Precondition Reasoning for Expected Run-Times of Probabilistic Programs*). Además de
calcular `ert[·]`, la herramienta genera las **obligaciones de prueba** asociadas a los
invariantes de los ciclos (`vcg[·]`, al estilo VCGen/lógica de Hoare) y las **verifica
automáticamente** con el SMT-solver Z3 (vía la librería `SBV`), reportando un contraejemplo
cuando un invariante propuesto no es válido.

Nace como una memoria de título (lenguaje basado en el lenguaje WHILE del curso *Análisis y
Verificación de Programas* CC7126-1, Otoño 2021 — <https://pleiad.cl/teaching/cc7126>) y sigue
en desarrollo activo: la rama actual generaliza la aritmética de lineal a polinomial y agrega
síntesis asistida de invariantes. Ver `CLAUDE.md` para el historial detallado sesión a sesión;
este documento es un resumen del estado actual.

## Gramática concreta

Expresiones aritméticas (`aexp`) — el AST interno soporta productos genuinos entre dos `AExp`
cualesquiera (`AExp :*: AExp`, normalizados internamente a forma polinomial canónica), pero la
sintaxis concreta sigue exponiendo sólo ponderación por un racional, igual que la versión
lineal original — para multiplicar dos variables entre sí hay que construir el AST a mano (ver
`app/Main.hs`) en vez de vía el parser:

```
aexp := n                 constante racional (n o n/m)
      | x                 variable
      | n * aexp
      | aexp + aexp
      | aexp - aexp        azúcar sintáctica
```

Expresiones aritméticas probabilistas (distribuciones sobre expresiones aritméticas):

```
paexp := n * <aexp>
       | paexp + paexp
```

Expresiones booleanas (`bexp`):

```
bexp := true | false
      | aexp <= aexp | aexp == aexp
      | aexp >= aexp | aexp > aexp | aexp < aexp | aexp != aexp   azúcar sintáctica
      | ! bexp
      | bexp && bexp | bexp || bexp
```

Expresiones booleanas probabilistas — muestra de una Bernoulli(p):

```
pbexp := <p>        con p en [0, 1]
```

Tiempos de ejecución (`RunTime`) — la indicatriz `[bexp]` es un `RunTime` que vale 0 o 1 y nada
más (constructor `RunTimeBExp`); para ponderarla por algo (o para multiplicar dos `RunTime`
cualesquiera en general) se usa `**`, un operador infijo genérico, no sólo "constante a la
izquierda":

```
runtime := aexp
         | [bexp]
         | runtime ++ runtime      suma
         | runtime -- runtime      resta (azúcar sintáctica)
         | runtime ** runtime      multiplicación genérica (ej. "[b]**2", "2**y", "[b]**y")
         | (runtime)
```

Programas (`program`) — el invariante de un ciclo es **opcional**: se puede escribir un
`while`/`pwhile` sin invariante (para completarlo después, a mano o vía síntesis) omitiendo el
bloque `{inv = ...}`/`{pinv = ...}` por completo:

```
program := empty | skip
         | identifier := aexp
         | identifier :~ paexp
         | program ; program
         | if (bexp) {program} else {program}
         | pif (<p>) {program} pelse {program}
         | it (bexp) {program}                        azúcar sintáctica (if sin else)
         | pit (<p>) {program}                         azúcar sintáctica (pif sin pelse)
         | while (bexp) {inv = runtime} {program}
         | while (bexp) {program}                      sin invariante, a completar después
         | pwhile (<p>) {pinv = runtime} {program}
         | pwhile (<p>) {program}                       ídem
         | for (n) {program}                            azúcar sintáctica (Seq repetido n veces)
```

### Definición de la transformada `ert[·]`

```
ert[C](f) =
  match C
    empty                          -> f
    skip                           -> 1 + f
    x := arit                      -> 1 + f[x -> arit]
    x :~ Σ p_i * arit_i            -> 1 + Σ p_i * f[x -> arit_i]
    C_1 ; C_2                      -> ert[C_1](ert[C_2](f))
    if (b) {C_1} else {C_2}        -> 1 + [b]*ert[C_1](f) + [!b]*ert[C_2](f)
    pif (<p>) {C_1} else {C_2}     -> 1 + p*ert[C_1](f) + (1-p)*ert[C_2](f)
    while (b) {C'} [I]             -> I                  -- requiere invariante I concreto
    pwhile (<p>) {C'} [I]          -> I                  -- ídem
```

### Obligaciones de prueba (`vcg[·]`)

```
Obligation := RunTime <= RunTime

VC[C](f) =
  match C
    empty, skip, x:=arit, x:~parit  -> {}
    C_1 ; C_2                       -> VC[C_1](ert[C_2](f)) ∪ VC[C_2](f)
    if/pif                          -> VC[C_1](f) ∪ VC[C_2](f)
    while (b) {C'} [I]              -> { 1 + [b]*ert[C'](I) + [!b]*f <= I } ∪ VC[C'](f)
    pwhile (<p>) {C'} [I]           -> { 1 + p*ert[C'](I) + (1-p)*f <= I } ∪ VC[C'](f)
```

A esto se suma la restricción de **buena-definición** de cada invariante (`0 <= I`, ver
`CLAUDE.md`) y, cuando el invariante es una plantilla con coeficientes libres, la cuantificación
`∃`(coeficientes) `∀`(variables de programa) se resuelve directo con Z3 en vez del viejo método
de "negar y probar por contradicción" (ver `completeRoutine'` más abajo).

## Mapa de módulos (`runtime/src/`)

- `Imp.hs` — ASTs (`AExp`, `BExp`, `RunTime`, `PAExp`/`PBExp`, `Program`), azúcar sintáctica,
  sustitución, simplificación/normalización.
- `ImpParser.hs` — parser Parsec de la sintaxis concreta de arriba.
- `ImpVCGen.hs` — `vcg[·]`: recorre un `Program` y arma las obligaciones de prueba; clasifica
  variables existenciales/universales; arma el input para SBV.
- `ImpSBV.hs` — traduce a `SBV`/Z3; cuantificación universal de cantidad arbitraria de
  variables (`mkUniversales`).
- `ImpSynth.hs` — **síntesis**: propone automáticamente un invariante-plantilla (*template
  natural*) para cada ciclo escrito sin invariante, a partir de la forma del programa. Ver
  `SINTESIS_TEMPLATE_NATURAL.md`.
- `ImpIO.hs` — formatea el output para consola; dos modos (`completeRoutine`/`completeRoutine'`,
  ver más abajo).
- `ImpProgram.hs` — banco de programas de ejemplo/test tomados de la memoria.

Otros directorios:

- `runtime/app/Main.hs` — entry point interactivo.
- `runtime/app-informe/InformeExamples.hs` — ejecutable (`informe-examples`) que corre el modo
  nuevo sobre ~30 programas del banco, citando la página/figura del informe correspondiente.
- `runtime/test/` — specs de Hspec (`cabal test runtime-test` / `stack test`).
- `material2020/` — material de la propuesta de memoria original (2020).

## Cómo compilar

El proyecto compila y pasa sus tests **tanto con `stack` como con `cabal`** (`package.yaml` es
la fuente de verdad; `runtime.cabal` se genera desde ahí vía `hpack` — si tocás
dependencias/build-tools, editá `package.yaml`).

Requisitos: Z3 (cualquier versión reciente; probado con 4.8.12) y `stack` o `cabal`+`ghcup`.

```bash
sudo apt-get install z3

cd runtime

# con stack
stack build
stack test

# con cabal
cabal build
cabal test runtime-test
```

`hie.yaml` apunta a un cradle de `stack` para HLS.

## Ejemplos de uso interactivo

Desde `runtime/`, cargar el módulo `Main` (`stack ghci Main.hs` o `cabal repl runtime-exe`):

```haskell
-- Modo antiguo: prueba cada contexto por separado, negando y buscando contraejemplo.
run "while(c == 1){inv = 1 ++ 4**[c == 1]}{c :~ 1/2* <0> + 1/2* <1>}"

-- Modo nuevo: un problema de SBV por invariante, cuantificación ∃∀ real vía mkUniversales.
run' "while(c == 1){inv = 1 ++ 4**[c == 1]}{c :~ 1/2* <0> + 1/2* <1>}"

-- Iteración de punto fijo de Kleene (útil para *adivinar* a mano la forma de un
-- invariante): fp x0 guarda cuerpo continuación n
fp "0" "x==0" "x:=x-1" "0" 3

-- Ídem para pwhile
fpp "0" "<1/2>" "skip" "3" 5
```

Los programas de prueba de la memoria están en `src/ImpProgram.hs` (ej. `run cpvcMenos`). Para
correr el banco completo con el modo nuevo y ver a qué página/figura del informe corresponde
cada uno: `cabal run informe-examples` (o `stack exec informe-examples` tras `stack build`).

## Referencias

- Introducción a SMT-solvers: <http://homepage.divms.uiowa.edu/~ajreynol/pres-iowa2017-part1.pdf>
- Andrew Reynolds: <http://homepage.cs.uiowa.edu/~ajreynol/>
- Trabajo guía de Tikhon Jelvis: <https://jelv.is/talks/compose-2016/>
- Tutorial SBV: <https://www.youtube.com/watch?v=gWZbNc5hqOA&list=PLfzJKXh_D71Rg8Cbl81sCzx59RloCspL->
- Sobre stack: <https://docs.haskellstack.org/en/stable/README/>
- Tutorial de parsers (Parsec): <https://jakewheat.github.io/intro_to_parsing/#an-issue-with-token-parsers>
