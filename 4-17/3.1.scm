(define make-accumulator (lambda(x)
	(lambda (add)
		(set! x (+ x add))
		x) ))
