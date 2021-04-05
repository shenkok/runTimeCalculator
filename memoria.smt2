; Expresiones Aritméticas
;Definicion de expresion aritmética determinista

(declare-datatypes () ((Arit    (Var (var-x String))
                                (Number (number-k Real)) 
                                (Weight (w-k Real) (w-arit Arit))
                                (Add (add-l Arit) (add-r Arit))
                                (Sub (sub-l Arit) (sub-r Arit))
                                )))

; funcion que permite sustituir una expresion aritmética for  en una expresion aritmética in
; subsArit :: String -> Arit -> Arit -> Arit
(define-fun-rec  subsArit ((var String)  (arit-for Arit) (arit-in Arit)) Arit( 
  match arit-in (
    ((Var x) (ite (= var x) 
                  arit-for
                  (Var x)
                  ))
    ((Number N) (Number N))
    ((Weight k arit) (Weight k (subsArit var arit-for arit )))
    ((Add arit-l arit-r) (Add (subsArit var arit-for arit-l) (subsArit var arit-for arit-r)))
    ((Sub arit-l arit-r) (Sub (subsArit var arit-for arit-l) (subsArit var arit-for arit-r)))
    )))


;;;;;;;;;;;;;;;;;;;;;;;;; código para practicar ;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;

(simplify (var-x (Var "x")))
(display (Add (Var "y") (Var "x")))

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
(define-fun-rec  subsDExp ((var String) (arit-for Arit) (dexp-in DExp)) DExp( 
  match dexp-in (
    (True True)
    (False False)
    ((Eq arit-l arit-r) (Eq (subsArit var arit-for arit-l) (subsArit var arit-for arit-r)))
    ((Geq arit-l arit-r) (Geq (subsArit var arit-for arit-l) (subsArit var arit-for arit-r)))
    ((Not dexp) (subsDExp var arit-for dexp))
    ((And d-l d-r) (And (subsDExp var arit-for d-l) (subsDExp var arit-for d-r)))
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

(define-fun-rec  subsRunTime ((var String)  (arit-for Arit) (runt-in RunTime)) RunTime( 
  match runt-in (
    ((RunTimeArit arit-rt) (RunTimeArit (subsArit var arit-for arit-rt)))
    ((Mult dexp runt-m) (Mult (subsDExp var arit-for dexp) (subsRunTime var arit-for runt-m)))
    ((Weight cte runt-w) (Weight cte (subsRunTime var arit-for runt-w)))
    ((Add runt-l runt-r) (Add (subsRunTime var arit-for runt-l) (subsRunTime var arit-for runt-r)))
    ((Sub runt-l runt-r) (Sub (subsRunTime var arit-for runt-l) (subsRunTime var arit-for runt-r)))
    ((Subs x arit-s runt-s) (Subs x
                            (subsArit x arit-for arit-s)
                            (ite (= var x)
                                    runt-s
                                    (SubsRunTime var arit-for runt-s))))
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
