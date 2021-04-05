; Expresiones Aritméticas
;Definicion de expresion aritmética determinista

(declare-datatypes () ((Arit    (Var (var-x String))
                                (Number (number-k Real)) 
                                (Weight (w-k Real) (w-arit Arit))
                                (Add (add-l Arit) (add-r Arit))
                                (Sub (sub-l Arit) (sub-r Arit))
                                )))

; funcion que permite sustituir una expresion aritmética 2 en una expresion aritmética 1
; subsArit :: String -> Arit -> Arit -> Arit
(define-fun-rec  subsArit ((var String) (arit1 Arit) (arit2 Arit)) Arit( 
  match arit1 (
    ((Var x) (ite (= var x) 
                  arit2
                  (Var x)
                  ))
    ((Number N) (Number N))
    ((Weight k arit) (Weight k (subsArit var arit arit2 )))
    ((Add arit-l arit-r) (Add (subsArit var arit2 arit-l) (subsArit var arit2 arit-r)))
    ((Sub arit-l arit-r) (Sub (subsArit var arit2 arit-l) (subsArit var arit2 arit-r)))
    )))


;;;;;;;;;;;;;;;;;;;;;;;;; código para practicar ;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;

(simplify (var-x (Var "x")))
(display (add (Var "y") (Var "x")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Definicion de expresiones booleanas deterministicas
(declare-datatypes () ((DExp    (True)
                                (False)
                                (Eq (eq-l Arit) (eq-r Arit))  
                                (Geq (geq-l Arit) (geq-r Arit))
                                (Not (no-dexp DExp))
                                (And (dexp-l DExp) (dexp-r DExp))
                                )))

; funcion que permite sustituir una expresion aritmética en una expresion booleana determinista
; subsDExp :: String -> DExp -> Arit -> DExp
(define-fun-rec  subsDExp ((var String) (dexp DExp) (arit Arit)) DExp( 
  match dexp (
    (True True)
    (False False)
    ((Eq arit-l arit-r) (Eq (subsArit var arit-l arit) (subsArit var arit-r arit)))
    ((Geq arit-l arit-r) (Geq (subsArit var arit-l arit) (subsArit var arit-r arit)))
    ((Not n-dexp) (subsDExp var n-dexp arit))
    ((And d-l d-r) (And (subsDExp var d-l arit) (subsDExp var d-r arit)))
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Definición de Tiempos de ejecución

;Definicion de RunTime
(declare-datatypes () ((RunTime     (RunTimeArit (runt-arit Arit))
                                    (Mult (mult-dexp DExp)(mult-runt RunTime))
                                    (Weight (w-k Real) (w-runt RunTime))
                                    (Add (add-l RunTime) (add-r RunTime))
                                    (Sub (sub-l RunTime) (sub-r RunTime))
                                    (Subs (subs-x String) (subs-arit Arit) (subs-runt RunTime))
                                    )))

; funcion que permite sustituir una expresion aritmética en un Runtime
; subsDExp :: String -> RunTime -> Arit -> RunTime
(define-fun-rec  subsRunTime ((var String) (runt RunTime) (arit Arit)) RunTime( 
  match runt (
    ((RunTimeArit arit-rt) (RunTimeArit (subsArit var arit-rt arit)))
    ((Mult dexp runt-m) (Mult (subsDExp var dexp arit) (subsRunTime var runt-m arit)))
    ((Weight cte runt-w) (Weight cte (subsRunTime var runt-w arit)))
    ((Add runt-l runt-r) (Add (subsRunTime var runt-l arit) (subsRunTime var runt-r arit)))
    ((Sub runt-l runt-r) (Sub (subsRunTime var runt-l arit) (subsRunTime var runt-r arit)))
    ((Subs x arit-s runt-s) (Subs x
                            ()
                             (ite (= var x)
                                    arit2
                                    (Var x))))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;; código para practicar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(simplify (var-x (Var "x")))
(simplify (add-r (Add (RunTimeArit (Var "y")) (RunTimeArit (Var "x")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Definicion de la funcion sustitucion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Definicion del lenguaje WHILE 
(declare-datatypes () ((Program     (Empty)
                                    (Skip)
                                    (Assigment (assig-var String) (assig-arit Arit))
                                    (Comp (comp-c1 Program) (comp-c2 Program))
                                    (If (if-dexp DExp) (if-c1 Program)(if-c2 Program))
                                    (While (while-dexp DExp) (while-c Program) (while-inv RunTime))
                                    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;funcion que permite sustituir un valor en una expresion artimética
; subsArit :: String -> Arit -> Arit -> Arit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
;Definicion de funcion  VC[C](f):: Program -> RunTime -> ( RunTime , [VerR] )
