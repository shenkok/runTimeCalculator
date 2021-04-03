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

Expresiones booleanas deterministicas :
        
      dξ  :=  true 
           |  false 
           | Arit = Arit 
           | Arit <= Arit 
           | ㄱ dξ
           | dξ ^ dξ


Programas :

      C := empty
         | skip
         | x := Arit
         | C ; C 
         | if (dξ) {C} else {C}
         | while (dξ) {C}

Tiempos de Ejecución (Runtime) :

      RunTime := Arit
               | [dξ] * Runtime
               | RunTime + Runtime
               | RunTime - Runtime
               | RunTime [x -> Arit]



Introducción a SMT-SOLVER:

      http://homepage.divms.uiowa.edu/~ajreynol/pres-iowa2017-part1.pdf

Andrew Reynols :

      http://homepage.cs.uiowa.edu/~ajreynol/

Cómo escribir de manera correcta :
    
      https://www.youtube.com/watch?v=XpgJ31GKPWI&list=PLyrlk8Xaylp5tLThZKObBuALYANlYzItz&index=2 

Link del informe 

      https://www.overleaf.com/2383766224twjygcsswxzw