# Síntesis de templates naturales (`fillTemplates`)

**Estado: implementado** en `runtime/src/ImpSynth.hs` (sección "TEMPLATES NATURALES"), con
tests en `runtime/test/ImpSynthSpec.hs` y punto de entrada interactivo `runSynth` en
`runtime/app/Main.hs`. Este documento es el diseño; ver "Resultado de la implementación" al
final para lo que se encontró al escribirlo.

Dado un `Program` con ciclos **sin invariante** (`While`/`PWhile` con `Nothing`), propone
automáticamente un invariante-plantilla para cada hueco, dejando el programa listo para
`vcGenerator'`.

Inspirado en los *natural templates* de Batz et al., *Probabilistic Program Verification via
Inductive Synthesis of Inductive Invariants* (TACAS 2023), §2.3 y Def. de pág. 418.

## Dónde encaja en el pipeline

```
Program con huecos (Nothing)
        │
        ▼
   fillTemplates          ← LO QUE FALTA (este documento)
        │
        ▼
Program con Just template
        │
        ▼
  vcGenerator' / programToSolverInput / mkUniversales      ← ya existe y funciona
        │
        ▼
  UNA sola consulta ∃(coeficientes) ∀(variables de programa) a Z3   ← sin CEGIS
        │
        ├── Satisfiable   → testigo: los valores de los coeficientes
        └── Unsatisfiable → "no se logró sintetizar un invariante con este template"
```

No hay lazo de refinamiento: un intento, un veredicto.

## La regla del template natural

En cada ciclo, el invariante propuesto tiene **dos piezas**:

| pieza | multiplicada por | ¿coeficientes frescos? |
|---|---|---|
| `[¬φ]` (o el peso `1-p` en un `pwhile`) | **la continuación** que la transformada ya calculó — ya viene parametrizada con los coeficientes de los niveles anteriores | **no**, se usa tal cual |
| `[φ]` (o el peso `p`) | una combinación **afín** de las variables relevantes del ciclo | **sí**, propios de este nivel |

La razón de que la pieza `[¬φ]` no lleve incógnitas: cuando la guarda es falsa, el valor del
invariante **ya se conoce exactamente** — es la continuación, por definición de `Φ_f`
(`Φ_f(I)(s) = f(s)` si `s ⊭ φ`). Gastar coeficientes ahí sería redundante, y fijarlo achica el
espacio de búsqueda.

Para un `pwhile(<p>)` no hay guarda booleana: la partición existe igual, pero pesada por las
**constantes** `1-p` / `p` en vez de por indicatrices — que es exactamente la forma en que
`vcGenerator'` ya arma su `l_inv`. Esta parte es una extensión nuestra: el lenguaje del paper
sólo tiene `while(φ){C}` con `φ` booleana, y la probabilidad entra únicamente como elección
*dentro* del cuerpo.

## Las dos direcciones de la recursión

`fillTemplates` recorre el programa igual que `vcGenerator'`, y como toda función recursiva
usa las dos direcciones del stack:

- **Bajando (argumento)**: la continuación `f`. Es lo que necesita la pieza `[¬φ]`.
- **Subiendo (resultado)**: el `ert` de este sub-programa. Es lo que necesita `Seq` para poder
  darle continuación al statement de la izquierda, y lo que necesitan las ramas de `If`/`PIf`.

O sea: hay que llegar al fondo de la llamada recursiva y reconstruir subiendo — pero la
continuación que consume la pieza `[¬φ]` viaja en la otra dirección. Ambas cosas conviven en la
misma pasada.

Como la pieza `[φ]` lleva coeficientes frescos (y **no** el valor que sube desde el cuerpo),
ningún template depende del valor ascendente de su propio cuerpo: no hay circularidad y cada
template cierra en el momento en que se construye.

## Firma

```haskell
-- f     : continuación de este programa (ya cerrada, viene bajando)
-- prog' : el mismo programa con los Nothing reemplazados por Just template
-- ert'  : el valor que este programa devuelve hacia arriba
fillTemplates :: Program -> RunTime -> Fresh (Program, RunTime)
```

`Fresh` es cualquier fuente de nombres nuevos (un `State Int`, o un contador sobre los nombres
ya usados con `freshName`, que ya existe en `ImpSynth.hs`).

## Algoritmo, caso por caso

Cada caso replica exactamente el `ert` que ya calcula `vcGenerator'`, y sólo agrega el relleno
de huecos.

### Casos sin huecos (sólo propagan el `ert`)

```haskell
fillTemplates Skip       f = pure (Skip,       rtOne :++: f)
fillTemplates Empty      f = pure (Empty,      f)
fillTemplates (Set x e)  f = pure (Set x e,    rtOne :++: sustRunTime x e f)
fillTemplates (PSet x d) f = pure (PSet x d,   rtOne :++: aexpE d x f)
```

### Composición secuencial — acá es donde hay que "tocar fondo y volver"

```haskell
fillTemplates (Seq p1 p2) f = do
  (p2', f2) <- fillTemplates p2 f     -- primero el de la derecha
  (p1', f1) <- fillTemplates p1 f2    -- su ert es la continuación del de la izquierda
  pure (Seq p1' p2', f1)
```

### Condicionales — las dos ramas comparten la misma continuación

```haskell
fillTemplates (If b pt pe) f = do
  (pt', ft) <- fillTemplates pt f
  (pe', fe) <- fillTemplates pe f
  pure ( If b pt' pe'
       , rtOne :++: ((RunTimeBExp b :**: ft) :++: (RunTimeBExp (Not b) :**: fe)) )

fillTemplates (PIf ber pt pe) f = do
  (pt', ft) <- fillTemplates pt f
  (pe', fe) <- fillTemplates pe f
  let pt_ = p ber
  pure ( PIf ber pt' pe'
       , rtOne :++: ((rtLit pt_ :**: ft) :++: (rtLit (1 - pt_) :**: fe)) )
```

### Ciclo determinista — el hueco

```haskell
fillTemplates (While b body Nothing) f = do
  tphi <- freshAffine (rmdups (freeVarsBExp b ++ freeVarsProgram body))
  let notB = simplifyBExp (Not b)                       -- ver "Cuidados" abajo
      t =  (RunTimeBExp notB :**: (rtOne :++: f))       -- ¬φ × continuación
       :++: (RunTimeBExp b   :**: (rtOne :++: tphi))    -- φ  × coeficientes frescos
  (body', _) <- fillTemplates body t   -- el cuerpo recibe como continuación el propio invariante
  pure (While b body' (Just t), t)     -- ert[while] = I  →  eso es lo que sube
```

El `_` no es descuido: para un ciclo, el valor que sube es **su propio invariante**, no lo que
devuelve el cuerpo — es la regla `ert[while(φ){C'}][I] = I` del informe, y es exactamente lo que
ya hace `vcGenerator'` (`runtime (vcGenerator' (While _ _ minv) runt) = requireInvariant minv`,
independiente de `runt`).

Distribuir el `+1` dentro de las dos piezas es equivalente a dejarlo afuera
(`1 ++ ([¬φ]**f ++ [φ]**tphi)`), porque las indicatrices particionan el espacio: exactamente
una vale 1.

### Ciclo probabilista — mismo esquema, pesos constantes

```haskell
fillTemplates (PWhile ber body Nothing) f = do
  tphi <- freshAffine (rmdups (freeVarsProgram body))
  let pt_ = p ber
      t =  (rtLit (1 - pt_) :**: (rtOne :++: f))
       :++: (rtLit pt_      :**: (rtOne :++: tphi))
  (body', _) <- fillTemplates body t
  pure (PWhile ber body' (Just t), t)
```

### Ciclos que el usuario ya anotó a mano

Se respetan tal cual, pero hay que seguir bajando: el cuerpo puede tener huecos.

```haskell
fillTemplates (While b body (Just t)) f = do
  (body', _) <- fillTemplates body t
  pure (While b body' (Just t), t)
-- ídem PWhile
```

### `freshAffine`

```haskell
freshAffine :: Names -> Fresh RunTime
-- vars = variables relevantes del ciclo: freeVarsBExp guarda ++ freeVarsProgram cuerpo
-- devuelve:  a_0 ++ a_1**v_1 ++ ... ++ a_n**v_n   con nombres frescos
```

Los nombres se generan con `freshName` (ya existe en `ImpSynth.hs`) contra el conjunto de
nombres ya usados — variables del programa **y** coeficientes ya asignados en otros niveles —
para que `getExistencialAndUniversalVars` los clasifique como existenciales sin ambigüedad.

## Cuidados (encontrados probando a mano, no hipotéticos)

1. **Simplificar la guarda antes de negarla.** Escribir `Not b` crudo cuando `b` ya es azúcar
   de una negación (ej. `x > 0` = `Not (x <= 0)`) deja una doble negación sin simplificar, y la
   impresión de resultados revienta con `error "No hay versión directa a AExp[!(!(x <= 0.0))]"`
   (`ImpVCGen.hs:292`). Hay que pasar por `simplifyBExp`.
2. **Producto existencial × universal.** Un término `a*x` (coeficiente existencial por variable
   universal) es aritmética real no lineal cuantificada — cara para Z3 en general. En los casos
   probados anduvo rápido, pero es el riesgo ya documentado en `CLAUDE.md`, sección "AExp: de
   lineal a polinomial".
3. **La heurística existencial/universal.** `getExistencialAndUniversalVars` clasifica por
   sintaxis; un ciclo cuya guarda no menciona una variable puede clasificarla mal (caso
   `Cdvc-`). Vale igual para los coeficientes generados acá.

## Alcance de esta vía — CERRADO acá

Decisión tomada: **esta vía llega hasta proponer una expresión afín con la estructura de los
natural templates, y no más**. O sea, exactamente

```
[¬φ] ** continuación   ++   [φ] ** (expresión afín en las variables)
```

- **Grado**: sólo afín (grado 1), igual que los natural templates del paper.
- **Partición**: sólo por la guarda del propio ciclo.
- **Fallo**: si Z3 no encuentra testigo, se reporta y se termina. Un intento, un veredicto.

### Refinar el template: descartado

**No se va a refinar el template.** No es que esté pendiente: está descartado, y por una razón
de fondo, no de esfuerzo.

Todo esquema de refinamiento (particionar por las ramas `if` del cuerpo, subdividir el espacio
de estados, poner los bordes de las piezas como incógnitas, usar la última instancia
parcialmente admisible como pista) es **CEGIS encubierto**: un lazo que propone, fracasa,
aprende algo del fracaso y vuelve a proponer, sólo que disfrazado de heurística sintáctica.
Si el problema requiere un lazo de ese tipo, la respuesta correcta es **hacer CEGIS explícito**
y aprovechar sus garantías, no reconstruirlo por partes dentro de una arquitectura one-shot
que no fue pensada para eso.

Así que el eje de partición queda cerrado. El único eje abierto es el grado — ver abajo.

## Validación previa (hecha a mano antes de escribir el código)

Los cuatro casos se armaron con los constructores existentes y se resolvieron con
`completeRoutine'` **sin modificar código de producción**:

| caso | template | resultado |
|---|---|---|
| `x:=3; while(x>0){x:=x-1}` (forma de `cdkcMenos`/`cdkcMas`) | `[x≤0]**1 ++ [x>0]**(1+a·x+b)` | válido, `a=3, b=3` |
| `while(c==1){c:~coin}` (familia `p4_6`/`p4_8`/`p4_9`) | `[c≠1]**1 ++ [c==1]**(1+a·c+b)` | válido, `a=5, b=0.125`; el álgebra da la condición `a+b ≥ 4`, que **es la misma familia** que el informe documenta (`K=4` exacta, `K=3` no alcanza, `K=5` floja) |
| `while(y≤x ∧ x≤z){x:=x+1/2}` (`cdvcMas`, 3 variables) | `[¬φ]**1 ++ [φ]**(1+ax·x+ay·y+az·z+a0)` | válido, `a0=3, ax=-4.5, ay=-0.5, az=5` (20 implicaciones) |
| `pwhile(<9/10>){while(c==1){c:~coin}}` (anidado, familia `Cpvc`) | dos niveles, la pieza `[c≠1]` del interno apunta al template del externo | válido, `a2=0, b2=61, aIn=60.19, bIn=-0.25`; `sharedExistentials` detectó las variables compartidas y resolvió los dos ciclos como **un solo sistema** |

En el anidado se probó además una variante donde el externo no lleva coeficientes propios sino
que se define como su propio `l_inv` (`1 ++ (1-p)**runt ++ p**T_inner`): también válida, con
menos incógnitas y una cota más ajustada (≈20/≈24 contra ≈545/≈5.5 del `cpvcMas` despejado a
mano en la memoria). Se descartó a favor de la regla uniforme (coeficientes en todos los
niveles) por ser más general.

## Resultado de la implementación

La versión automática (`runSynth "..."`, o `synthesizeTemplates0` seguido de
`completeRoutine'`) reproduce **exactamente** los mismos testigos que los cuatro experimentos
manuales de la tabla de arriba, partiendo sólo del programa sin invariantes. Los coeficientes
se llaman `a0, a1, ...` en vez de los nombres ad-hoc de los experimentos.

Además:

- **La guarda conjuntiva no necesitó De Morgan**: para `cdvcMas`, la pieza `[¬φ]` queda como
  `Not (y<=x && x<=z)` y el resto de la maquinaria la procesa sin problema. Sólo hacía falta
  `simplifyBExp` para la doble negación de las guardas azucaradas.
- **El caso "cuerpo con más statements que el ciclo interno"** (la duda que había quedado
  abierta) funciona: `while(x>0){y:=x; while(y>0){y:=y-1}; x:=x-1}` se procesa entero. Como su
  costo es **cuadrático** en `x` y el template es afín, el veredicto es "no existe una
  asignación de las variables de template que la satisfaga" — el modo de fallo esperado,
  reportado y sin refinar.

### Bug pre-existente encontrado y corregido: `ImpVCGen.hs`, `restrictionsToImplications`

```haskell
-- antes
simplify_runtime = deepSimplifyRunTime runtimeA --: runtimeB
-- después
simplify_runtime = deepSimplifyRunTime (runtimeA --: runtimeB)
```

La aplicación de función liga más fuerte que el operador, así que la versión vieja se leía como
`(deepSimplifyRunTime runtimeA) --: runtimeB`: **sólo se simplificaba el lado izquierdo de la
restricción, el invariante no**. Y `deepSimplifyRunTime` es justo lo que normaliza la aritmética
dentro de las indicatrices (`RunTimeBExp bexp -> RunTimeBExp (deepSimplifyBExp bexp)`), así que
una condición sin normalizar de ese lado (`x + -1*1 <= 0` en vez de `-1 + x <= 0`) no matcheaba
contra los átomos ya normalizados del otro, `evalCondition` no la reconocía, sobrevivía a la
evaluación de contextos y `runTimeToArit` fallaba con `"No hay versión directa a AExp"`.

Nunca se había disparado porque todos los invariantes del banco están escritos a mano en forma
ya normalizada; aparece apenas un invariante lo genera una sustitución, que es exactamente lo
que hace `fillTemplates`. Es el mismo tipo de bug de paréntesis ya documentado en `CLAUDE.md`
para `vcGenerator'`. Corregirlo no cambió ningún veredicto existente (los 212 tests previos
siguen pasando).

### Subir el grado — la única vía abierta

**Esta es la continuación elegida** cuando el template afín no alcanza: no refinar la partición,
sino subir el grado del polinomio. Se probó una vez y se revirtió, pero no por ser la dirección
equivocada — por el costo del solver, que está medido abajo y hay que atacar antes de retomarla.

Ante el fallo del cuadrático se probó generalizar `freshAffine` a `freshPolynomial d` (un
coeficiente por cada monomio de grado ≤ d, o sea C(n+d, d) coeficientes para n variables) y
parametrizar toda la cadena por el grado. Funciona mecánicamente, pero:

| caso | coeficientes | variables ∀ | resultado |
|---|---|---|---|
| `while(x>0){x:=x-1}` | 3 | `x` | **válido** en segundos: `3 + 0.25·x + 3·x²` |
| `while(x>0){y:=x; while(y>0){y:=y-1}; x:=x-1}` | 9 | `x, y` | **timeout > 5 min**, sin veredicto |

El template del segundo caso era, para el ciclo externo,
`[x≤0]**1 ++ [x>0]**(1 + a0 + a1·x + a2·y + a3·x² + a4·x·y + a5·y²)`, y para el interno
`[y≤0]**(1 + 1 + ⟨externo con x:=x-1⟩) ++ [y>0]**(1 + a6 + a7·y + a8·y²)`. Nueve existenciales
contra dos universales, con monomios `x²`, `x·y`, `y²`: aritmética real **no lineal con
alternancia de cuantificadores**, decidible (Tarski) pero doblemente exponencial. Es el riesgo
que ya estaba anotado en `CLAUDE.md` desde el refactor de `AExp` a polinomial, ahora con un caso
concreto que lo confirma.

Nótese la asimetría: sobre ese mismo programa, el template **afín** responde `Unsatisfiable` en
segundos. Descubrir que el grado 1 no alcanza es barato; confirmar que el grado 2 sí alcanza es
lo caro.

Se revirtió y el código quedó **sólo afín**, pero la vía sigue siendo ésta. Las dificultades
concretas a resolver antes de retomarla, en orden:

1. **Timeout de Z3 vía `SMTConfig`.** Hoy un template caro *cuelga* en vez de devolver "no se
   pudo determinar". Sin esto, cualquier experimento de grado ≥ 2 es inusable: no distinguís
   "no existe instancia" de "todavía está pensando". Es el prerequisito de todo lo demás.
2. **Achicar el problema.** Dos palancas independientes: subir el grado **sólo en el ciclo que
   lo necesita** (no uniformemente en todos los niveles), y **restringir la base de monomios**
   — potencias puras (`x²`, `y²`) sin los cruzados (`x·y`), que es lo que más infla la cuenta
   `C(n+d, d)` y lo que más le cuesta a `nlsat`.
3. **Asumir la asimetría.** Descubrir que un grado no alcanza seguirá siendo barato
   (`Unsatisfiable` en segundos); confirmar que el siguiente sí alcanza seguirá siendo caro.
   Eso no se arregla, se administra: conviene subir de a un grado, con timeout, y aceptar
   "no se pudo determinar" como veredicto legítimo.

El límite teórico de fondo no se mueve: `∃(coeficientes) ∀(variables de programa)` sobre
aritmética real **no lineal** es decidible (Tarski) pero doblemente exponencial. La vía del
grado funciona en casos chicos y se degrada rápido — es la que hay, con los ojos abiertos.

### Sobre `certifyDegree` (eliminado)

`ImpSynth.hs` tenía una segunda vía de síntesis, anterior a esta: certificar el **grado** de un
`RunTime` por diferencias finitas simbólicas, con una consulta `∃c ∀x. Δᵈf(x) = c` a Z3 (si la
d-ésima diferencia es constante, `f` es polinomio de grado `d`). Funcionaba y estaba testeada,
incluida la regla de uso de que un iterado de Kleene sólo es confiable hasta su profundidad (más
allá, la meseta del truncamiento rompe la diferencia constante, así que hay que acotar la región).

Se **eliminó** al cerrar esta sesión, porque el camino que quedó es el de templates naturales
afines, que no necesita certificar grado: propone afín y listo. Mantener las dos vías en
paralelo era cargar código sin usar. **Si hace falta recuperarla**, está completa en el commit
`cf6407d` (`git show cf6407d:runtime/src/ImpSynth.hs`), junto con sus 15 tests en
`git show cf6407d:runtime/test/ImpSynthSpec.hs`.

### Tests agregados

`test/ImpSynthSpec.hs` suma 10 casos (6 estructurales sin Z3, 4 de punta a punta con Z3):
relleno de huecos, respeto de un invariante escrito a mano, ciclos anidados, la regresión de la
doble negación en la pieza `[¬φ]`, colisión de nombres de coeficientes con variables del
programa, y los cuatro veredictos end-to-end (incluido el cuadrático que debe fallar).

Total del proyecto: **207 examples, 0 failures, 5 pending**, con `cabal test runtime-test` y
con `stack test` (eran 222 antes de eliminar los 15 tests de `certifyDegree`).

---

# Vía 2: iteración de punto fijo sobre la estructura afín de `ert`

**Estado: análisis, no implementado** (sesión 2026-09-20). **Queda como trabajo futuro**
(decidido 2026-10-05): antes se refinan los simplificadores de `AExp`/`BExp`/`RunTime`. Es una vía *alternativa* a los
templates, no un refinamiento de ellos: donde aplica, calcula el invariante exacto **sin Z3 y
sin adivinar**. Cubre una clase de programas distinta a la de la Vía 1, y notablemente incluye
casos que en la memoria hubo que despejar a mano.

Hay una versión presentable de la primera parte de este análisis (el desglose constructor por
constructor) en `https://claude.ai/artifact/X5SbygvbrdSiodamjG7g7h`.

## 1. El hecho estructural: `ert[C]` es afín en la continuación

Para todo `C` sin ciclos, `ert[C](f) = ert[C](0) + wp[C](f)` — costo propio más parte lineal.
Verificado caso por caso contra `ImpVCGen.hs:52-63`:

| constructor | `ert[C](f)` en el código | costo `c_C` | lineal `L_C(f)` |
|---|---|---|---|
| `Empty` | `f` | `0` | `f` |
| `Skip` | `1 + f` | `1` | `f` |
| `Set x e` | `1 + f[x:=e]` | `1` | `f[x:=e]` |
| `PSet x d` | `1 + E_d[f]` | `1` | `Σ pᵢ·f[x:=vᵢ]` |
| `If b C₁ C₂` | `1 + [b]·ert₁ + [¬b]·ert₂` | `1 + [b]c₁ + [¬b]c₂` | `[b]·L₁(f) + [¬b]·L₂(f)` |
| `PIf p C₁ C₂` | `1 + p·ert₁ + (1−p)·ert₂` | `1 + p·c₁ + (1−p)c₂` | `p·L₁(f) + (1−p)·L₂(f)` |
| `Seq C₁ C₂` | `ert₁(ert₂(f))` | `c₁ + L₁(c₂)` | `L₁ ∘ L₂` |
| `While`/`PWhile` | `I` (el invariante) | — | — |

Cuatro hechos elementales lo sostienen: sustituir es lineal, la esperanza es lineal,
multiplicar por una **función fija** (una indicatriz, un peso constante) es lineal *en `f`*, y
componer afines da afín. Inducción estructural sobre `Program` y listo.

**La columna de la derecha es `wp[C]`.** No es un concepto nuevo que haya que agregarle a la
herramienta: es el nombre de la mitad de `ert` que depende de `f`. Nunca se ve en el código
porque `vcGenerator` calcula `c + L(f)` como una sola expresión.

**Esta parte es robusta.** Ni siquiera una asignación no afín (`x := x*y`) la rompe: la
linealidad es *en `f`*, y sustituir distribuye sobre sumas sin importar qué tan fea sea la
expresión que se sustituye. Lo único que la rompería es **no-determinismo** (`wp` demoníaco es
un `min`, que no es lineal) — que este lenguaje no tiene.

`While`/`PWhile` son la excepción, y no por casualidad: `vcGenerator` devuelve el invariante e
**ignora `runt`**. Es el punto donde la herramienta deja de computar y empieza a suponer;
la obligación de prueba `Φ(I) ⊑ I` es la deuda que salda esa suposición.

## 2. Consecuencia: la iteración de Kleene es una serie de Neumann

El funcional característico (`cfWhile`, `ImpVCGen.hs:175`) hereda la forma afín:

```
Φ(X) = a + L(X)      a    = Φ(0)   = 1 + [¬φ]·f + [φ]·c_C
                     L(X) = Φ(X) − a = [φ]·L_C(X)
```

Ambas piezas son computables con lo que ya existe — `a = cfWhile b body f rtZero` y
`L(v) = cfWhile b body f v --: a`. No hace falta implementar `L` por separado.

Y un funcional afín iterado desde el fondo produce **sumas parciales de una serie**:

```
xₙ = Φⁿ(0) = a + L(a) + L²(a) + … + Lⁿ⁻¹(a)            lfp = Σ_{k≥0} Lᵏ(a)
```

Serie de Neumann (la geométrica, para operadores). La convergencia deja de ser una pregunta
sobre programas y pasa a ser una pregunta sobre **el radio espectral de `L`**.

Interpretación: `Φⁿ(0)` no es una aproximación abstracta — **es el tiempo esperado exacto del
ciclo que se rinde después de `n` vueltas** (y al rendirse no cobra nada, que es el `0` del que
se parte). Cada `Lᵏ(a) = x_{k+1} − x_k` es lo que gana una vuelta más de paciencia.

### Desglose verificado sobre `p4_6`

`while(c == 1){ c :~ ½<0> + ½<1> }`, con `Φ(X) = 1 + [c==1]·(1 + E[X])`:

```
a     = 1 + [c==1]
L(X)  = [c==1]·E[X]
Lᵏ(a) = 3·(½)ᵏ · [c==1]      para k ≥ 1
```

| n | término `Lⁿ⁻¹(a)` | suma parcial `xₙ` | salida real de `fp` |
|---|---|---|---|
| 1 | `1 + [c==1]` | `1 + 1·[c==1]` | `1.0 ++ [c == 1.0]` |
| 2 | `(3/2)[c==1]` | `1 + (5/2)[c==1]` | `… ** 2.5` |
| 3 | `(3/4)[c==1]` | `1 + (13/4)[c==1]` | `… ** 3.25` |
| 4 | `(3/8)[c==1]` | `1 + (29/8)[c==1]` | `… ** 3.625` |
| 5 | `(3/16)[c==1]` | `1 + (61/16)[c==1]` | `… ** 3.8125` |
| 6 | `(3/32)[c==1]` | `1 + (125/32)[c==1]` | `… ** 3.90625` |

La tercera columna es la salida literal de
`fp "0" "c==1" "c :~ 1/2* <0> + 1/2* <1>" "0" n` (el `examplePresentacion` que ya está en
`app/Main.hs`). Forma cerrada `Kₙ = 4 − 3·(½)ⁿ⁻¹`, límite `4`, o sea `1 ++ 4**[c==1]`.

El `3` de cada término se decodifica como `2 + 1`: **una vuelta más** (guarda + cuerpo) para
los que siguen, más **la guarda de salida** para los que ya terminaron y que la truncación
anterior nunca alcanzó a ejecutar. Verificado: `½·2 + ½·1 = 3/2` ✓.

Y la distancia al punto fijo es exactamente la cola: `4 − Kₙ = 3(½)ⁿ⁻¹ = Σ_{k≥n} Lᵏ(a)`.

## 3. La órbita, la base finita y el lema del estancamiento

La pregunta "¿cierra la serie?" es: **¿la órbita `{Lᵏ(a)}` vive en un subespacio de dimensión
finita?** Los `RunTime` son funciones del estado a los reales, o sea vectores; el espacio
ambiente es de dimensión infinita, pero la órbita puede quedar atrapada en un subespacio chico.

Se construye incrementalmente, `V₁ ⊆ V₂ ⊆ V₃ ⊆ …` con `V_k = span{a, L(a), …, L^{k-1}(a)}`
(subespacios de Krylov). Y vale el

> **Lema del estancamiento.** Si `Lᵏ(a) ∈ V_k`, entonces `V_j = V_k` para todo `j > k`.
>
> Prueba: si `Lᵏ(a) = Σ cᵢ·Lⁱ(a)` con `i < k`, aplicando `L` queda
> `L^{k+1}(a) = Σ cᵢ·L^{i+1}(a)`, y todos los de la derecha ya están en `V_k`. Inducción. ∎

**Por eso el chequeo es finito y decidible**: basta que *una sola vez* el iterado nuevo sea
linealmente dependiente de los anteriores. No hay que verificar infinitos pasos. (En la
literatura: el paso donde se estanca es el grado del polinomio mínimo de `L` relativo a `a`.)

## 4. Taxonomía de programas

| clase | condición | qué se puede hacer |
|---|---|---|
| **C0** | la órbita es finita, `Φⁿ` se estabiliza literal | Kleene termina; nada que adivinar |
| **C1** | la órbita vive en dimensión finita | **álgebra lineal: invariante exacto, sin solver** |
| **C2** | la órbita es una familia parametrizada sumable | sumación simbólica (geométrica, Faulhaber, Gosper) |
| **C3** | el resto | template + ∃∀ (la Vía 1) |

Dos condiciones garantizan C1, y son justamente los dos modos de escape:

1. **Los átomos booleanos no crecen.** Es lo que da `PSet` sobre soporte finito: sustituye
   literales concretos, así que todo átomo que dependa de esa variable **colapsa a
   `True'`/`False'`** — nunca fabrica uno nuevo. Es lo que rompe `x := x-1` sobre `[x>0]`, que
   genera `[x>1]`, `[x>2]`, … para siempre.
2. **El grado aritmético no crece.** Es lo que dan las asignaciones **afines** (`x := ax+b`):
   sustituir una afín en un polinomio de grado `d` deja grado `d`. Los polinomios de grado ≤ d
   en `n` variables son un subespacio de dimensión finita — `C(n+d, d)`, que es exactamente el
   número de coeficientes de `freshPolynomial d` del experimento de grado 2. No es casualidad:
   **es la dimensión de ese subespacio**.

### El banco, clasificado

| programas | forma | clase |
|---|---|---|
| `p4_1`, `cpkcMas`, `cpkcMenos` | `pwhile(<p>){skip}` | **C1**, dim 1 |
| `p4_2`, `p4_6`–`p4_9` | `while(c==1){c:~coin}` | **C1**, dim 2 |
| `p4_3`, `cpvcMas`, `cpvcMenos`, `cpvc` | `pwhile(<9/10>){ while(c==1){c:~coin} }` | **C1**, dim 2 |
| `cdvcMenos` | `while(false){skip}` | C1 degenerado (`L = 0`) |
| `p2_1`, `cdkcMenos/Mas`, `p4_10`–`p4_15` | `while(x>0){x:=x-1}` | **C2** |
| `p2_2` | `while(y>=10){y:=y-1; x:=x+1}` | **C2** |
| `cdvcMas` | `while(y<=x && x<=z){x:=x+½}` | **C2** |
| anidado cuadrático | `while(x>0){y:=x; while(y>0){…}; x:=x-1}` | **C2** |

El anidado `Cpvc` es C1 y conviene justificarlo porque sorprende: el `wp` del ciclo interno es
`[c≠1]·X + [c==1]·X[c:=0]`, y si `X ∈ span{1,[c==1]}` entonces `X[c:=0]` es una constante, así
que **el resultado se queda en el mismo span de dimensión 2**. El `pwhile` externo no aporta
átomos (su "guarda" es una probabilidad).

**El corte no es por dificultad aparente, es por determinista vs. probabilista.** Todo el lado
probabilista del banco es C1; todo el lado de contador determinista es C2. Y eso invierte la
dificultad respecto de la memoria: **`Cpvc` —el caso que hubo que despejar a mano con la
hipótesis de `K`, y que en la herramienta necesitó `sharedExistentials` para resolver los dos
ciclos como un sistema conjunto— se resolvería invirtiendo una matriz de 2×2.** Tiene sentido a
posteriori: muestrear de una distribución de soporte finito **destruye información de estado**,
y eso mantiene la dimensión baja; un contador determinista la preserva y la desplaza.

## 5. El algoritmo

```
FASE 0 — partir el funcional
    a    := cfWhile b body f rtZero
    L(v) := cfWhile b body f v --: a

FASE 1 — base de Krylov, con presupuesto N
    B := [a];  v := a
    repetir hasta N:
        v := L(v)
        si v es combinación lineal de B  →  ESTANCÓ: base cerrada, salir (C1)
        si no                            →  B := B ++ [v]
    presupuesto agotado → caer a la Vía 1 (template + ∃∀)

FASE 2 — coordenadas
    base canónica átomo × monomio (ej. {1, [c==1]})
    â      := coordenadas de a
    col j de M := coordenadas de L(Bⱼ)

FASE 3 — resolver
    El invariante es punto fijo:  I = a + L(I)
    En coordenadas eso ES un sistema lineal:   (Id − M)·v = â
    Gaussiana sobre Rational. Exacto, sin punto flotante, sin solver.
    I := Σ vᵢ·Bᵢ

FASE 4 — certificar
    I queda como invariante CONCRETO (sin existenciales) → vcGenerator + completeRoutine',
    que es el camino barato por contradicción que ya anda rápido con todo el banco.
```

Es un **semi-decisor correcto**: cuando el lema del estancamiento dispara, el veredicto es
definitivo (no hay falsos positivos); cuando no, simplemente se usa la Vía 1.

### `p4_6` completo

```
Base {1, [c==1]}:
    a = 1 + [c==1]                          →  â = (1, 1)
    L(1)      = [c==1]                      →  columna (0, 1)
    L([c==1]) = ½·[c==1]                    →  columna (0, ½)

    M = ⎡0  0⎤      (Id − M)·v = â:   v₁ = 1
        ⎣1  ½⎦                        −v₁ + ½v₂ = 1  →  v₂ = 4

    ρ(M): autovalores 0 y ½  →  ρ = ½ < 1  ✓

    I = 1 + 4·[c==1]                        ← el invariante documentado del banco
```

Atajo cuando el estancamiento es escalar (`L^{k+1}(a) = r·Lᵏ(a)`): ni hace falta la matriz,
`Σ Lᵏ(a) = a + L(a)/(1−r)`.

## 6. El chequeo `ρ(M) < 1` y su trampa

La condición de convergencia **no es sobre el determinante** (error fácil de cometer). Es sobre
el radio espectral:

```
‖M‖ < 1   ⟹   ρ(M) < 1   ⟺   converge   ⟹   |det M| < 1
(suficiente)              (exacta)         (necesaria, NO suficiente)
```

Contraejemplo del determinante: `M = diag(2, 0.1)` tiene `det = 0.2 < 1` y diverge.

**Y el atajo de la norma falla en nuestro propio caso**: la `M` de `p4_6` tiene una fila que
suma `1.5 > 1`, y sin embargo `ρ = ½`. La razón es que **la norma depende de la base y el radio
espectral no** — la base `{1, [c==1]}` no es una base de probabilidades.

**La trampa concreta**: si `1` es autovalor, `(Id − M)` es singular y la gaussiana avisa sola;
pero si `ρ(M) > 1` sin que `1` lo sea, el sistema **igual tiene solución única** — un punto fijo
que **no es el mínimo**, mientras el verdadero vale `∞` en algún estado. Un número creíble y
equivocado. El chequeo no es opcional.

### Lo que garantiza la semántica

`L(X) = [φ]·wp[C](X)` es un operador **positivo y subestocástico**: `L(1) ≤ 1`, porque
`wp[C](1)` es una probabilidad de terminación y `[φ]` sólo puede matar masa. De ahí
**`ρ(L) ≤ 1` siempre** — el caso explosivo no puede pasar en esta semántica.

Ojo con el matiz: `L(1) ≤ 1` **no** acota los valores (`L(1000)` puede valer casi mil). Acota el
**factor de amplificación**. Es perfectamente compatible con que los `RunTime` vivan en `[0,∞]`:
el infinito entra por la puerta de la **suma infinita**, no de ningún paso individual — igual
que `1 + 1 + 1 + … = ∞` sin que ningún término explote.

Queda entonces un único modo de falla:

| | significado |
|---|---|
| `ρ(L) < 1` | se fuga masa hacia la salida → **tiempo esperado finito**, la serie cierra |
| `ρ(L) = 1` | hay región donde no se fuga → **tiempo esperado infinito** |

O sea que el chequeo es, de yapa, **el certificado de terminación en expectativa** (PAST).
El caso canónico de `ρ = 1` es expresable en este lenguaje y sería un buen test del borde:

```
while(x > 0){ x :~ 1/2 * <x-1> + 1/2 * <x+1> }
```

La caminata aleatoria simétrica: termina con probabilidad 1 (AST) pero su tiempo esperado es
infinito (no PAST). En la semántica de `ert` el `lfp` existe igual y vale `∞`; lo que no puede
representar el infinito es el vector de coordenadas — se rompe el álgebra lineal, no la teoría.

## 7. C2: la identidad de capas y el `+1` de grado

A las indicatrices que proliferan **no se las acota, se las cambia de base**. La familia
`[x>0], [x>1], [x>2], …` no es un conjunto arbitrario: es *una representación de un polinomio*.

```
Σ_{k≥0} [x > k]  =  x            (layer cake / Fubini discreto)
```

Desarrollado sobre `cdkcMenos` (`Φ(X) = 1 + [x>0]·(1 + X[x:=x-1])`), usando el colapso por
subsunción `[x>0]·[x>1] = [x>1]`:

```
x₁ = 1 + [x>0]
x₂ = 1 + 2[x>0] + [x>1]
x₃ = 1 + 2[x>0] + 2[x>1] + [x>2]
x₄ = 1 + 2[x>0] + 2[x>1] + 2[x>2] + [x>3]

xₙ = 1 + 2·Σ_{k=0}^{n-2} [x>k]  +  [x>n-1]
                                   └─ término de borde: se anula apenas n > x

lfp = 1 + 2·Σ_{k≥0}[x>k] = 1 + 2x        ← exactamente p2_1: `1 ++ 2**[x>0]**x`
```

**La sumación cuesta exactamente un grado** (Faulhaber: `Σ_{k<n} kᵈ` es de grado `d+1`). En
`cdkcMenos` el sumando era constante → grado 1. En el anidado cuadrático el sumando es lineal
en `k` (el ciclo interno cuesta ≈ `x−k` en la vuelta `k`) → **grado 2**.

O sea: **el experimento de subir el template a grado 2 no era arbitrario, estaba forzado por la
teoría.** Pero las dos rutas hacia ese grado 2 no cuestan lo mismo:

| ruta | qué hace | costo medido |
|---|---|---|
| template ∃∀ | **busca** 9 coeficientes con aritmética real no lineal cuantificada | timeout > 5 min |
| sumación simbólica | **calcula** `Σ_{k<n} g(k,x)` con Faulhaber | cerrado, aritmético |

La asimetría anotada más arriba ("confirmar que el grado 2 alcanza es lo caro") **desaparece si
se calcula la suma en vez de buscar los coeficientes**.

### Wald: cuándo alcanza "vueltas × costo"

La intuición `E[total] = E[vueltas] × costo de una vuelta` es la identidad de Wald, y necesita
que **todas las vueltas cuesten lo mismo**. En este lenguaje eso se traduce a: el cuerpo es
código recto (sin ciclo anidado, sin `if` con ramas de distinto largo).

- `p4_6`: `2 vueltas × 2 + 1 = 5` ✓  (`E[N] = 2`, constante sobre `c==1`)
- `cdkcMenos`: `x vueltas × 2 + 1 = 1 + 2x` ✓  (`E[N] = x` — **una función del estado**, no un
  número; sólo es escalar cuando el ciclo no depende del estado)
- anidado cuadrático: la vuelta `k` cuesta `≈ 2(x−k)+4`. No hay "el costo de una vuelta" que
  sacar factor común; `Σ [2(x−k)+4] = x(x+1)+4x`, cuadrático.

**"Se rompe Wald" y "hace falta subir el grado" son el mismo fenómeno dicho de dos maneras**:
costo constante → sale factor común, el grado no se mueve; costo variable → hay que sumar una
sucesión no constante, y ahí aparece el grado extra.

## 8. Banderas rojas para reconocer el escape

| # | señal sintáctica | qué rompe | ¿recuperable? |
|---|---|---|---|
| 1 | asignación no afín (`x := x*y`) | el grado explota (se duplica por paso) | no, ni con sumación |
| 2 | asignación autorreferente a variable de guarda (`x := x-1`) | los átomos proliferan | sí — C2, se suma |
| 3 | **ciclo anidado con cota dependiente del externo** | el costo por vuelta varía | sí — C2, sube un grado |
| 4 | `if` en el cuerpo con ramas de distinto largo | nada, en realidad | **no rompe nada acá** — ver abajo |

Sobre la fila 4: en la Vía 1 un `if` en el cuerpo obligaría a particionar el template a mano.
Acá **no hace falta ninguna heurística**: el átomo de la guarda del `if` simplemente aparece
como un vector más de la base, y si el conjunto de átomos sigue siendo finito la órbita sigue
cerrando. La base *descubre* la partición en vez de que haya que adivinarla. Es una de las
ventajas concretas de esta vía sobre la de templates.

**La trampa está en la fila 3**: las otras tres se ven mirando el AST, pero la 3 **no se ve
mirando ningún iterado**, porque cada iterado finito es piecewise-afín y sólo el límite es
cuadrático. Un recognizer basado en "inspeccioná el grado de los iterados" diría "grado 1, todo
bien" indefinidamente. Por eso el chequeo dinámico (iterar con presupuesto + testear dependencia
lineal) es el único confiable: detecta que la base *no cierra* sin necesitar entender por qué.
Las banderas sintácticas sirven para explicar y para filtrar rápido, no para decidir.

Distinción útil: **un producto en el programa no es lo mismo que un producto en la respuesta**.
Producto en una asignación (`x := x*y`) mata el método; producto en el invariante (`n*m`, `x²`)
es normal y esperado — es lo que pasa al sumar una familia, y es exactamente para lo que se
generalizó `AExp` de lineal a polinomial en esta rama.

Y la caracterización sintáctica *no hay que intentar completarla*: es suficiente pero no
necesaria (a `x := 1-x` sobre `[x==1]` se le escapa, y sin embargo su órbita es periódica de
período 2, así que cierra). La condición verdadera es "las variables de guarda recorren un
conjunto finito de valores alcanzables" — o sea, *finite-state en la parte del estado que tocan
las guardas*, que es la misma hipótesis bajo la cual `cegispro2` tiene completitud.

## 9. Subsunción: la regla angosta que hace falta

Para que los iterados de C2 no queden ilegibles hace falta que `[x>0]·[x>1]` colapse a `[x>1]`.
`buildMul` ya junta las indicatrices en una conjunción, pero `simplifyBExp` (`Imp.hs`) **no
tiene razonamiento de subsunción** — sólo resuelve constantes e igualdad sintáctica.

La regla que alcanza, y es aritmética de `Rational`, no un solver:

```
(p ≤ c₁) ∧ (p ≤ c₂)   ≡   p ≤ min(c₁, c₂)
(p ≤ c₁) ∨ (p ≤ c₂)   ≡   p ≤ max(c₁, c₂)
```

o sea: **mismo polinomio normalizado (módulo escalar positivo), distinta constante**. Con
`completeNormArit` ya está media hecha.

Y es exactamente la familia que genera la iteración, porque una asignación afín autorreferente
`x := ax+b` sobre un átomo lineal siempre produce **el mismo polinomio con la constante
corrida**. Vale incluso multivariado: en `cdvcMas` (`x := x+1/2` sobre `y−x ≤ 0` y `x−z ≤ 0`)
los polinomios quedan idénticos y sólo se mueve la constante.

**La incompletitud acá es segura**: no subsumir deja una expresión más grande, nunca una
respuesta incorrecta. Es optimización de tamaño, no condición de corrección — muy distinto del
paso de verificación, donde un `Unknown` es un veredicto perdido. Por eso conviene implementar
los casos baratos y parar, y **no** meter Z3 adentro de `simplifyBExp`: volverla
`BExp -> IO BExp` cambiaría la firma de todo lo que está aguas abajo, metería latencia en el
camino caliente y un tercer resultado posible en una función que hoy no puede fallar.

## 10. Lo único que falta implementar

De todo el algoritmo, el único ingrediente que no existe es la **extracción de coordenadas**:
dado un `RunTime`, descomponerlo canónicamente como `Σ cᵢ·(átomo × monomio)`, para poder
(a) testear dependencia lineal en la Fase 1 y (b) armar `M` y `â` en la Fase 2. `cfWhile`, la
aritmética racional, `completeNormArit` y toda la certificación ya están.

Esa descomposición es, otra vez, la **forma normal de `RunTime`** que la memoria deja como
trabajo futuro (§6.4) y que `CLAUDE.md` documenta como deuda técnica pre-existente. La
diferencia es que ahora tiene un uso concreto que la justifica, y que **no hace falta la forma
normal completa**: alcanza con decidir igualdad y dependencia lineal en el fragmento que
generan estas iteraciones.
