(add-load-path ".")
(load "throw-catch")

(define prod-list (lambda (nums)
	(define prod-list-iter (lambda (nums prod)
		(cond
			((null? nums) prod)
			((= (car nums) 0) (throw ZeroFindError 0))
			(else (prod-list-iter (cdr nums) (* prod (car nums))))) ))	
	(catch ZeroFindError (prod-list-iter nums 1)) ))
