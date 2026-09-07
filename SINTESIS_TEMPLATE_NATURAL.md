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

## Alcance de esta versión

- **Grado**: sólo afín (grado 1), igual que los natural templates del paper. Subir de grado se
  probó y se revirtió (ver más abajo).
- **Partición**: sólo por la guarda del propio ciclo. El paper también particiona por las ramas
  `if` internas del cuerpo (sus `B_i'`); no está acá.
- **Fallo**: si Z3 no encuentra testigo, se reporta y se termina. Sin refinamiento, sin CEGIS.

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

### Experimento: subir el template a grado 2 (probado y revertido)

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

Se revirtió: el código quedó **sólo afín**. Para retomarlo haría falta primero (a) ponerle
timeout a Z3 vía `SMTConfig`, para que un template caro devuelva "no se pudo determinar" en vez
de colgarse, y (b) achicar el problema — subir el grado sólo en el ciclo que lo necesita, o
restringir la base de monomios (potencias puras, sin cruzados).

### Cómo refina cegispro2, y por qué no nos sirve para el caso cuadrático

Del reporte extendido (Batz et al., **arXiv:2205.06152**, Apéndice D — *no* está en el PDF de
TACAS de 20 páginas, que sólo lo referencia). El punto central es que **cegispro2 nunca sube el
grado**: sus invariantes son piecewise linear de punta a punta. La gramática de templates (§3)
es literalmente

```
E → r | x | r·x | E + E
```

— no hay `x·y` ni `x²`. Toda la expresividad extra viene de agregar **más piezas**, cada una
todavía lineal:  `T = [B₁]·E₁ + ... + [Bₙ]·Eₙ`, con los `Bᵢ` particionando el espacio de estados.

El dilema que plantean: *"If T is too restrictive, it excludes admissible invariants... If T is
too liberal, the synthesizer has to search a high-dimensional space."* Por eso arrancan
optimistas, con un `T₁` chico, y refinan sólo si el synthesizer prueba que no hay instancia.

Las tres estrategias:

1. **Static Hyperrectangle Refinement** (sólo estado finito). Acotan cada variable → el espacio
   es un hiperrectángulo. `Tᵢ` parte cada dimensión en `i` pedazos iguales (hasta `i^|Vars|`
   piezas): si `T₁ = Σⱼ [Bⱼ]·Eⱼ` y los hiperrectángulos son `R₁…Rₘ`, entonces
   `Tᵢ = Σⱼ Σₖ [Bⱼ ∧ Rₖ]·E_{k,j}`.
2. **Dynamic Hyperrectangle Refinement**. Igual, pero los bordes de los hiperrectángulos son
   **variables de template** (no se fija dónde cortar). Es la variante *non-fixed-partition*.
3. **Inductivity-Guided Refinement**. Usa como pista la última instancia *parcialmente
   admisible* `I` que devolvió el synthesizer: parte cada `Bⱼ` en la región donde `I` sí es
   parcialmente inductiva (`Ψ_f(I) ⪯ I`) y donde no. La partición se computa simbólicamente.

Sobre garantías, son explícitos en que el refinamiento **no es monótono**: *"the approaches do
not yield step-wise refinements, i.e. ⟨Tᵢ⟩ ⊆ ⟨Tᵢ₊₁⟩, all approaches ensure progress, i.e.
⟨Tᵢ⟩ ⊊ ⟨Tᵢ₊ⱼ⟩ for some j ≥ 1. **For finite-state programs**, progress ensures completeness: we
eventually reach a maximally-partitioned template T in which every state has its own piece."*

**Y ahí está el límite que nos importa**: la completitud sale de que, en el límite, cada estado
tenga su propia pieza constante — eso sólo cierra si el espacio de estados es **finito**. Un
runtime genuinamente cuadrático (`n²`) **no es piecewise-linear sobre un dominio no acotado**:
ninguna cantidad finita de piezas lineales lo acota para todo `n`. Sus benchmarks son de estado
finito o acotados (BRP tiene `sent < 8·10⁶` en la guarda), y en la evaluación de UPAST (pág. 423
de la versión TACAS) restringen explícitamente a *"N-valued, linear programs with **flattened
nested loops**"* — o sea, aplanan justo la construcción que genera runtimes cuadráticos. Además
admiten que hay programas donde su método falla (`gridbig`, timeout en las tres estrategias).

Empíricamente (Tabla 2 del apéndice E.1): la **inductivity-guided gana casi siempre**; la
**dynamic hace timeout en casi todo** (`chain`, `zeroconf`, `brp`). Ellos mismos concluyen que
*"searching for good fixed-partition templates in a separate outer loop pays off"*.

**Conclusión para este proyecto**: refinar por particiones y subir el grado son **dos ejes
distintos**, y cegispro2 sólo recorre el primero. Para nuestro caso cuadrático haría falta el
segundo, con el costo medido más arriba. Si alguna vez se retoma el refinamiento, el orden
sensato sería:

1. Lo más barato y que ya tienen ellos en `T₁` y nosotros no: **particionar también por las
   ramas `if` del cuerpo del ciclo**, no sólo por la guarda. No cuesta nada de solver (sigue
   siendo lineal).
2. Después, algo estilo *inductivity-guided*, que es la única de las tres que **no necesita
   acotar variables** y reusa información que el solver ya produjo.
3. El grado, sólo con timeout de Z3 configurado y achicando el problema.

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
