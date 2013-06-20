(use gauche.test)
(add-load-path ".")

(test-start "3.54")

(load "3.54")

(test* "(stream-ref factorials 4)" 120 (stream-ref factorials 4))
(test* "(stream-ref factorials 6)" 5040 (stream-ref factorials 6))