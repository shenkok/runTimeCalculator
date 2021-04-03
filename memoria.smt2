; Expresiones Aritméticas
;Definicion de expresion aritmética determinista
(echo "starting Z3")

(declare-datatypes () ((Arit (Var (x String))
                             (Number (n Real)) 
                             (Mult (n-mult Real) (arit-mult Arit))
                             (Sum (r-sum Arit) (l-sum Arit))
                             (Sub (r-sub Arit) (l-sub Arit))
                             (Sus (r-sus Arit) (var-sus String) (l-sus Arit)))
                       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Definicion de expresiones booleanas deterministicas
(declare-datatypes () ((DExp True
                             False
                             (Eq (r-eq Arit) (l-eq Arit))  
                             (Geq (r-geq Arit) (l-geq Arit))
                             (Not (no-exp DExp))
                             (And (r-or DExp) (l-or DExp)))
                       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


