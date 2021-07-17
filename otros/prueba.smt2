(set-logic QF_LIA)
(declare-const x Int)
(declare-const y Int)

(define-fun conjetura_1 () Bool
    (=> ( > x 0) (> x 1 ))
)

(define-fun conjetura_2 () Bool
    (=> ( > x 0) (>= x 1 ))
)

(assert (not conjetura_2))
(check-sat)
(get-model)