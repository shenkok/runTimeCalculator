; Expresiones Aritméticas
;Definicion de expresion aritmética determinista
(echo "starting Z3")

(declare-datatypes () ((Arit    (Var (x String))
                                (Number (k Real)) 
                                (Mult (k Real) (arit Arit))
                                (Sum (r-arit Arit) (l-arit Arit))
                                (Sub (r-arit Arit) (l-arit Arit))
                                )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Definicion de expresiones booleanas deterministicas
(declare-datatypes () ((DExp    True
                                False
                                (Eq (r-arit Arit) (l-arit Arit))  
                                (Geq (r-arit Arit) (l-arit Arit))
                                (Not (no-exp DExp))
                                (And (r-dexp DExp) (l-dexp DExp))
                                )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Definición de Tiempos de ejecućión

;Definicion de RunTime
(declare-datatypes () ((RunTime     (RunT (rarit Arit))
                                    (Mult (dexp DExp)(runT RunTime))
                                    (Weight(k Real) (runT RunTime))
                                    (Sum (r-runT RunTime) (l-runT RunTime))
                                    (Sub (r-runT RunTime) (l-runT RunTime))
                                    (Sus (x String) (arit Arit) (runT RunTime))
                                    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                       

;Definicion del lenguaje WHILE 
(declare-datatypes () ((Program     Empty
                                    Skip
                                    (Assigment (r-assig String) (l-assig Arit))
                                    (Comp (c-1) (c-2))
                                    (Dif (dexp DExp) (c-1 Program)(c-2 Program))
                                    (While (dexp DExp) (c Program))
                        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Definicion de 
