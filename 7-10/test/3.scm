(use gauche.test)
(add-load-path ".")

(test-start "3")

(load "3")

(define us-coins '(25 10 5 1))
(define gb-coins '(50 20 10 5 2 1))
(test* "(change gb-coins 43) == '(20 20 2 1)" (change gb-coins 43) '(20 20 2 1))
(test* "(change us-coins 43) == '(25 10 5 1 1 1)" (change us-coins 43) '(25 10 5 1 1 1))
(test* "(change '(5 2) 16) == '(5 5)" (change '(5 2) 16) '(5 5 2 2 2))
