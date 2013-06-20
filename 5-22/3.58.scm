(add-load-path ".")
(load "stream.scm")

(define (expand num den radix)
	(cons-stream
			(quotient (* num radix) den)
			(expand (remainder (* num radix) den) den radix)))

(show-stream (expand 3 8 10) 20)