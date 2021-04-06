; Definicion de lista enlazada

(set-option :smt.mbqi true)
;version no correcta falta hacer el +1 
(define-fun-rec lenList ((l (List Int))) Int (
    match l (
    (nil 0)
    ((insert k  tail) (lenList tail)) 
    )))



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
                                    (RunTimeMult (mult-dexp DExp)(mult-runt RunTime))
                                    (RunTimeWeight (w-k Real) (w-runt RunTime))
                                    (RunTimeAdd (add-l RunTime) (add-r RunTime))
                                    (RunTimeSub (sub-l RunTime) (sub-r RunTime)) 
                                    (RunTimeSubs (subs-x String) (subs-arit Arit) (subs-runt RunTime))
                                    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; funcion que permite sustituir una expresion aritmética en un Runtime
; subsDExp :: String -> RunTime -> Arit -> RunTime
(define-fun-rec subsRunTime ((var String) (arit-for Arit) (runt-in RunTime)) RunTime (
    match runt-in (
    ((RunTimeArit arit) (RunTimeArit (subsArit var arit-for arit)))
    ((RunTimeMult dexp runt) (RunTimeMult (subsDExp var arit-for dexp) (subsRunTime var arit-for runt )))
    ((RunTimeWeight k runt)(RunTimeWeight k (subsRunTime var arit-for runt)))
    ((RunTimeAdd runt-l runt-r)(RunTimeAdd (subsRunTime var arit-for runt-l) (subsRunTime var arit-for runt-r)))
    ((RunTimeSub runt-l runt-r) (RunTimeSub (subsRunTime var arit-for runt-l) (subsRunTime var arit-for runt-r)))
    ((RunTimeSubs x arit runt) (RunTimeSubs x
                            (subsArit x arit-for arit)
                            (ite (= var x)
                                    runt
                                    (subsRunTime var arit-for runt))))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;; código para practicar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(simplify (var-x (Var "x")))

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

(declare-datatypes () ((Restriction   (ResGeq (rge-l RunTime) (rge-r RunTime))
                                    (ResEq (re-l RunTime) (re-r RunTime))

)))


(forall  ((x (List  Int)) (y (List  Int)))
    (= (append x y)
        (ite (= x (as nil (List  Int)))
            y
            (let ((h (head x)) (t (tail x)))
                (insert h (append t y))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
;Definicion de funcion  VC[C](f):: Program -> RunTime -> [Restriction]->( RunTime , [Restriction] )



(simplify(append (as nil (List Int)) (insert 6 (insert 5 (as nil (List Int))))))
(simplify (let ((x 6)) x ))



