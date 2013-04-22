(use gauche.test)
(add-load-path ".")

(test-start "3.1")

(load "3.1")
(define A (make-accumulator 5))

(test* "(A 10)" 15 (A 10))
(test* "(A 10)" 25 (A 10))
