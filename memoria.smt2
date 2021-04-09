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
    ((Not dexp) (Not (subsDExp var arit-for dexp)))
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
; funcion que permite sustituir una expresion aritmética en un RunTime
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


(define-sort Restrictions () (List Restriction))

; Calculo del largo de un conjunto de restricciones 
;lenRes(ress) :: Restrictions -> Int
(define-fun-rec lenRes ((ress Restrictions)) Int (
    match ress (
        (nil 0)
        ((insert k tail) (+ 1 (lenRes tail))) 
    )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; union entre dos conjuntos de restricciones
; unionRes(res1, res2):: Restrictions -> Restrictions -> Restrictions
(define-fun-rec unionRes ((res1 Restrictions)(res2 Restrictions)) Restrictions (
    match res1 (
        (nil res2)
        ((insert k tail) (unionRes tail (insert k res2))) 
    )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:
(declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))

;Definicion de funcion  VC[C](f):: Program -> RunTime ->( RunTime , Restrictions )
(define-fun-rec VC ((C Program) (runt RunTime)) (Pair RunTime Restrictions)(
    match C (
       (Skip (mk-pair
                (RunTimeAdd (RunTimeArit (Number 1.0)) runt)
                nil))
        (Empty (mk-pair
                runt
                nil))
        ((Assigment x arit) (mk-pair
                                (RunTimeAdd (RunTimeArit (Number 1.0)) (subsRunTime x arit runt))
                                nil))
        ((If cond ct cf ) (let ((S1 (VC ct runt )) (S2 (VC cf runt )))
                            (mk-pair
                                (RunTimeAdd
                                    (RunTimeArit (Number 1.0)) 
                                    (RunTimeAdd 
                                        (RunTimeMult cond (first S1)) 
                                        (RunTimeMult (Not cond) (first S2))))
                                (unionRes (second S1)(second S2)))
                            ))
        ((Comp c1 c2) (let ((S2 (VC c1 runt )))
                        (let ((S1 (VC c2 (first S2) )))
                            (mk-pair (first S1) (unionRes (second S1) (second S2))) 
                        )))
        ((While cond c i)(let ((S (VC c runt )))
                            (mk-pair
                                i (insert 
                                    (ResGeq
                                    (RunTimeAdd 
                                    (RunTimeArit(Number 1.0))
                                    (RunTimeAdd 
                                    (RunTimeMult cond (first S))
                                    (RunTimeMult (Not cond) runt))) i)
                                    (second S)
                                    ))
                            ))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-const a Int)

(assert (= a (lenRes (insert (ResGeq (RunTimeArit (Number 3.0)) (RunTimeArit (Number 3.0)))(as nil Restrictions)))))
(assert (= a 1.0))
(check-sat)
(get-model)

