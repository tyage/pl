(add-load-path ".")
(load "throw-catch")

(define (change coins amount)
	(cond ((zero? amount) '())
		((null? coins) (throw Fail #f))
		(else
			(let ((c (car coins)))
				(if (> c amount) (change (cdr coins) amount)
					(or (catch Fail
						(cons c (change coins (- amount c))))
						(change (cdr coins) amount) ))))))

