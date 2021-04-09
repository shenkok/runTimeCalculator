;Definicion de Dado de n caras
(declare-datatypes(T) ((Dice  end
                             (add (hd Pair) (tl Dice)))
                       (Pair (mk-pair (value T) (p Real)) ) 
                       ))

(define-fun-rec countFace ((x (Dice Real))) Int (
  match x (
    (end 0)
    ((add head tail) (+ 1 (countFace tail)))
  )
))
(simplify (countFace (add (mk-pair 0.0 0.0)(add (mk-pair 1.0 1.0) (as end (Dice Real))))))