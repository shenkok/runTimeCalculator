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
           |  Arit = Arit 
           |  Arit <= Arit 
           |  ㄱ dξ
           |  dξ ^ dξ
           |  dξ [x -> Arit]


Tiempos de Ejecución (Runtime) :

      RunTime := Arit
               | [dξ]*RunTime
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
                empty -> f                
                skip -> 1 + f
                x := μ -> 1 + f[x -> μ]
                C_1 ; C_2 -> ert[C_1](ert[C_2](f))
                if (dξ) {C_1} else {C_2} -> 1 + [dξ]*ert[C_1](f) + [~dξ]*ert[C_2](f)
                while (dξ) {C'} [I] -> lfp X. 1 + [~dξ]*f + [dξ]*ert[C'](X) <= I  

Restricción para verificar :

      VerR := RunTime <= RunTime
              RunTime == RunTime

Definición de función generadora de restricciones  VC(C)(f):: Program -> RunTime -> {VerR}

      VC[C](f) = 
               match C
               empty -> {}
               skip -> {}
               x := μ -> {}
               C_1 ; C_2 -> VC[C_1](ert[C_2](f)) Union VC[C_2](f)
               if (dξ) {C_1} else {C_2} -> VC[C_1](f) Union VC[C_2](f)
               while (dξ) {C'} [I] -> { 1 + [dξ]*ert[C'](I) + [~dξ]*f <= I } Union VC[C'](f)



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

Documentación

      http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2017-07-18.pdf

Ejemplo de lista enlazada

      (display (insert 3 ( insert 3 (as nil (List Int) ))))

Ejemplo de uso let 

            (simplify
            (let ((rt1 (RunTimeArit (Number 1.0))) (rt2 (RunTimeArit (Number 4.0))))
                  (let ((res1  (ResGeq rt1 rt2)) (res2 (ResEq rt2 rt1)))
                        (let ((n (lenRes (insert res1 (insert res2 (as nil Restrictions))))))
                        n
                        ))))

Ejemplo tutorial:

     https://sat-smt.codes/

Otro ejemplo :       

      https://www.lri.fr/~conchon/TER/2013/2/SMTLIB2.pdf

rise4fun ;

      https://rise4fun.com/Z3/tutorial/guide

link reu;

      https://uchile.zoom.us/j/88041295290?pwd=RC9PNzYrOVhOTjdmRkIraXVNbDlVQT09