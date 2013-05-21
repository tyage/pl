(use gauche.test)
(add-load-path ".")

(test-start "3.33")

(load "3.33")

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(define d (make-connector))
(constant 10 a)
(constant 5 b)
(averager a b c)
(averager a d b)

(test* "(averager a b c)" (/ 15 2) (get-value c))
(test* "(averager a d b)" 0 (get-value d))