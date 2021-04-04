# runTimeCalculator
Repositorio Memoria 


Lenguaje basado en el lenguaje WHILE del curso Análisis y Verificación de Programas CC7126-1 - Otoño 2021 Clase 2
link del curso :

    https://pleiad.cl/teaching/cc7126

Nota: Para la realización de esta memoria por el momento sólo se trabaja con la sintaxis del lenguaje.

Expresiones aritméticas :

      Arit := n  constante real
            | x  variable
            | n * Arit
            | Arit + Arit 
            | Arit - Arit 
            | Arit [x -> Arit]

Expresiones booleanas deterministicas :
        
      dξ  :=  true 
           |  false 
           | Arit = Arit 
           | Arit <= Arit 
           | ㄱ dξ
           | dξ ^ dξ
           | dξ [x -> Arit]


Tiempos de Ejecución (Runtime) :

      RunTime := Arit
               | [dξ] * RunTime
               | RunTime + RunTime
               | RunTime - RunTime
               | RunTime [x -> Arit]

Programas :

      C := empty
         | skip
         | x := Arit
         | C ; C 
         | if (dξ) {C} else {C}
         | while (dξ) {C} [RunTime]

Definición de transformada ert[C](f):: Program -> RunTime -> RunTime 

      ert[C](f) = 
                match C
                Skip -> 1 + f
                Empty -> f
                x := μ -> 1 + f[x/ μ]
                C_1 ; C_2 -> ert[C_1](ert[C_2](f))
                if (dξ) {C_1} else {C_2} -> 1 + [dξ] * ert[C_1](f) + [~dξ] * ert[C_2](f)
                while (dξ) {C} [I] -> lfp X. 1 + [dξ] * f + 



Primer ciclo while de prueba:
      C_geo :

      while (c = 1)
            {c := 1/2 *< 0 > + 1/2 * < 1 >}

Invariante asociado al ciclo C_{geo}:

      C <= 1 + 4*[c = 1]


Introducción a SMT-SOLVER:

      http://homepage.divms.uiowa.edu/~ajreynol/pres-iowa2017-part1.pdf

Andrew Reynols :

      http://homepage.cs.uiowa.edu/~ajreynol/

Cómo escribir de manera correcta :
    
      https://www.youtube.com/watch?v=XpgJ31GKPWI&list=PLyrlk8Xaylp5tLThZKObBuALYANlYzItz&index=2 

Link del informe 

      https://www.overleaf.com/2383766224twjygcsswxzw