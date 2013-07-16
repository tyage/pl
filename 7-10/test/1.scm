(use gauche.test)
(add-load-path ".")

(test-start "1")

(load "1")

(test* "(prod-list '(3 2 1)) == 6" (prod-list '(3 2 1)) 6)
(test* "(prod-list '(3 0 1)) == 0" (prod-list '(3 0 1)) 0)
(test* "(prod-list '(353 22 1000)) == 6" (prod-list '(353 22 1000)) 7766000)
