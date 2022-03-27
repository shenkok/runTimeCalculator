# runTimeCalculator
Repositorio Memoria 


Lenguaje basado en el lenguaje WHILE del curso Análisis y Verificación de Programas CC7126-1 - Otoño 2021 Clase 2
link del curso :

    https://pleiad.cl/teaching/cc7126

Nota: Para la realización de esta memoria por el momento sólo se trabaja con la sintaxis del lenguaje.

Expresiones aritméticas deterministas :

      Arit := n  constante racional
            | x  variable
            | n * Arit
            | Arit + Arit

Expresiones aritméticas Probabilistas :

      PArit := Σ p_i * Arit   con p_i en [0, 1]  Distribución sobre expresiones aritméticas

            
Expresiones booleanas deterministicas :
        
      dξ  :=  true, false   constantes booleanas
           |  Arit = Arit   igualdad de expresiones aritméticas
           |  Arit <= Arit  menor igual de expresiones aritméticas
           |  ㄱ dξ         negación
           |  dξ ^ dξ       and lógico
           |  dξ v dξ       or lógico

Expresiones booleanas Probabilistas :

      pξ  :=  ber(p) con p en [0, 1] Muestra de distribución bernoulli 

Tiempos de Ejecución (Runtime) :

      RunTime := Arit
               | [dξ]*RunTime
               | RunTime + RunTime
               | n*RunTime
Programas :

      C := empty
         | skip
         | x := Arit
         | x :~ PArit
         | C ; C 
         | if (dξ) {C} else {C}
         | pif (pξ) {C} else {C}
         | pwhile (pξ) {C} [RunTime]
         | while (dξ) {C} [RunTime]

Definición de transformada ert:: Program -> RunTime -> RunTime 

      ert[C](f) = 
                match C
                empty                          -> f                
                skip                           -> 1 + f
                x := arit                      -> 1 + f[x -> arit]
                x :~ Σ p_i * arit_i            -> 1 + Σ p_i*f[x -> arit_i]
                C_1 ; C_2                      -> ert[C_1](ert[C_2](f))
                if (dξ) {C_1} else {C_2}       -> 1 + [dξ]*ert(C_1)(f) + [~dξ]*ert[C_2](f)
                pif (ber(p)) {C_1} else {C_2}  -> 1 + p*ert[C_1](f) + (1 - p)*ert[C_2](f)
                while  (dξ) {C'} [I]           -> I
                pwhile (pξ) {C'} [I]           -> I

Obligaciones de prueba :

      Obligation := RunTime <= RunTime

Definición de función generadora de restricciones  VC(C)(f):: Program -> RunTime -> {VerR}

      VC[C](f) = 
               match C
               empty                         -> {}
               skip                          -> {}
               x := arit                     -> {}
               x :~ Σ p_i * arit_i           -> {}
               C_1 ; C_2                     -> VC[C_1](ert[C_2](f)) Union VC[C_2](f)
               if (dξ) {C_1} else {C_2}      -> VC[C_1](f) Union VC[C_2](f)
               pif (ber(p)) {C_1} else {C_2} -> VC[C_1](f) Union VC[C_2](f)
               while (dξ) {C'} [I]           -> { 1 + [dξ]*ert[C'](I) + [~dξ]*f <= I } Union VC[C'](f)
               pwhile (pξ) {C'} [I]          -> { 1 + p*ert[C'](I) + (1-p)*f <= I } Union VC[C'](f)



Introducción a SMT-SOLVER:

      http://homepage.divms.uiowa.edu/~ajreynol/pres-iowa2017-part1.pdf

Andrew Reynols :

      http://homepage.cs.uiowa.edu/~ajreynol/



Trabajo guía de Tikhon Jelvis
      
      https://jelv.is/talks/compose-2016/

tutorial sbv

      https://www.youtube.com/watch?v=gWZbNc5hqOA&list=PLfzJKXh_D71Rg8Cbl81sCzx59RloCspL-&index=9

sobre stack:

      https://docs.haskellstack.org/en/stable/README/

tutorial parser
    
    https://jakewheat.github.io/intro_to_parsing/#an-issue-with-token-parsers




------------------------------------------------------------------------------------------------------------------------------------------------------------

Sobre la distribución de archivos

El trabajo principal se encuentra en la carpeta `runtime`

Con respecto a la otra carpeta: 


- `material2020`: Contiene el material usado y generado en la propuesta de memoria (2020).


Sobre la carpeta `runtime`: El proyecto es hecho en stack versión 2.7.3 y ocupa el smt-solver Z3 version 4.8.7 - 64 bit. Es posible usar la herramienta sin Z3, pero sólo para los programas sin ciclos. 

- `app`: Contiene el archivo `Main.hs`, que es el archivo a ejecutar en el proyecto.

- `src`: Contiene los diferentes módulos usados en el proyecto.
    - `src\Imp.hs` Módulo con la implemetación del lenguaje imperativo.
    - `src\ImpSBV` Módulo para generar las variables de SBV.
    - `src\ImpIO` Módulo con las funciones necesarias para producir un output entendible de los procedimientos.
    - `src\ImpParser` Módulo con la implementación del parser. 
    - `src\ImpProgram` Módulo con programas escritos en sintáxis concreta.
    - `src\ImpVCGen` Módulo con las funciones asociadas a las funciones VCGen.

- `package.yaml`: Contiene las librerías necesarias para compilar el proyecto

--------------------------------------------------------------------------------------------------------------------------------------------------------

Como compilar:
- Instalar `Z3` (Version 4.8.7) `sudo apt-get install z3`
- Instalar  `stack` (Version 2.7.3) ejecutar  ` curl -sSL https://get.haskellstack.org/ | sh`
- Para instalar el trabajo ejecutar  `stack build ` en la carpeta  `runtime`.
- Para cargar un módulo en consola (con todas sus variables), ir a la carpeta y usar  `stack ghci Modulo.hs `.
- Para ver en consola sólo el output de  `Main.hs` (en este caso el cálculo hecho sobre el `CTrunc`), ejecutar  `stack exec runtime-exe`.
- **Importante** Cada vez que se edita algún archivo usar  `stack build`

Ejemplo interactivo:
- Ir a la carpeta  `runtime\app` y ejecutar  `stack ghci Main.hs `
- Ejecutar `run "tu_programa"`
- Un ejemplo es `run "while(c == 1){inv = 1 ++ 4**[c == 1]}{ c :~ 1/2* <0> + 1/2* <1>}"`
- Los programas usados como  test se encuentran definidos en  `src\ImpProgram`.
- Es posible ejecutarlos con el desde el módulo `Main.hs`, un ejemplo es `run cpvcMenos`.