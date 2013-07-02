(define (cons x y) (lambda (m) (m x y)))

(define (car z) (z (lambda (p q) p)))

(define (cdr z) (z (lambda (p q) q)))

(define (length l) (if (null? l) 0 (+ 1 (length (cdr l)))))

(length (cons (/ 1 0) (cons 3 '())))

(car (cons 3 (/1 0)))

