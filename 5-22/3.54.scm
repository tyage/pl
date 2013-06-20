(add-load-path ".")
(load "stream.scm")

(define mul-streams (lambda (s1 s2)
	(stream-map * s1 s2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
	(stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials (cons-stream 1 (mul-streams factorials 
	(add-streams ones integers))))
