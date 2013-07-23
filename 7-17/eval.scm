(define true #t)
(define false #f)

;;; evaluator in CPS with continuation as concrete data
;;; Sec 4.1.1
(define (eval exp env cont)
  (cond
   ((self-evaluating? exp) (apply-cont cont exp))
   ((variable? exp) (apply-cont cont (lookup-variable-value exp env)))
   ((quoted? exp) (apply-cont cont (text-of-quotation exp)))
   ((assignment? exp) (eval-assignment exp env cont))
   ((definition? exp) (eval-definition exp env cont))
   ((if? exp) (eval-if exp env cont))
   ((or? exp) (eval (or->if exp) env cont))
   ((let? exp) (eval (expand-let exp) env cont))
   ((lambda? exp)
    (apply-cont 
     cont
     (make-procedure (lambda-parameters exp)
		     (lambda-body exp)
		     env)))
   ((begin? exp) 
    (eval-sequence (begin-actions exp) env cont))
   ((cond? exp) (eval (cond->if exp) env cont))
;; Changes for CATCH-THROW begins
   ((catch? exp)
    (eval (catch-tag exp) env (make-cabodyc (catch-body exp) env cont)))
   ((throw? exp)
    (eval (throw-tag exp) env (make-thbodyc (throw-body exp) env cont)))
;; Changes for CATCH-THROW ends
   ((application? exp)
    (eval (operator exp) env
	  (make-operandsc (operands exp) env cont)))
   (else
    (error "Unknown expression type -- EVAL" exp))))

(define (my-apply procedure arguments cont)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments cont))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))
	   cont))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (apply-cont cont val)
  (cond ((haltc? cont) (list 'normal val))
	((testc? cont)
	 (if (true? val)
	     (eval (testc-true cont) (testc-env cont) (testc-cont cont))
	     (eval (testc-false cont) (testc-env cont) (testc-cont cont))))
	((assignc? cont)
	 (set-variable-value! (assignc-var cont) val (assignc-env cont))
	 (apply-cont (assignc-cont cont) 'ok))
	((definec? cont)
	 (define-variable! (definec-var cont) val (definec-env cont))
	 (apply-cont (definec-cont cont) 'ok))
	((beginc? cont)
	 (eval-sequence
	  (beginc-rest-exps cont)
	  (beginc-env cont)
	  (beginc-cont cont)))
	((operandsc? cont)
	 (list-of-values (operandsc-exps cont) 
			 (operandsc-env cont)
			 (make-applyc val (operandsc-cont cont))))
	((applyc? cont)
	 (my-apply (applyc-proc cont) val (applyc-cont cont)))
	((restopsc? cont)
	 (list-of-values (restopsc-rest cont) (restopsc-env cont)
			 (make-consc val (restopsc-cont cont))))
	((consc? cont)
	 (apply-cont (consc-cont cont)
		     (cons (consc-val cont) val)))
;;; Changes for CATCH-THROW begins
	((cabodyc? cont)
	 (eval-sequence (cabodyc-body cont) (cabodyc-env cont)
			(make-catchc (cons val (caadr cont)) (cabodyc-cont cont))))
	((catchc? cont)
	 (apply-cont (catchc-cont cont) val))
	((thbodyc? cont)
	 (eval (thbodyc-body cont) (thbodyc-env cont)
	       (make-throwc val (thbodyc-cont cont))))
	((throwc? cont)
	 (let ((stripped-cont
		(first-matching-catch (throwc-tag cont) (throwc-cont cont))))
	   (if stripped-cont
	       ((lambda () 
	       	(define c (applyc-cont (consc-cont (restopsc-cont (throwc-cont cont)))))
	       	(define handler (catchc-handler c))
	       	(my-apply 
	       		(make-procedure (lambda-parameters handler) (lambda-body handler) the-global-environment)
	       		(list val) c) ))
	       (list 'uncaught (throwc-tag cont) val))))
;;; Changes for CATCH-THROW ends
	(else (error "Unknown continuation type -- APPLY-CONT" cont))
))

(define (list-of-values exps env cont)
  (if (no-operands? exps)
      (apply-cont cont '())
      (eval (first-operand exps) env
	    (make-restopsc (rest-operands exps) env cont))))

(define (eval-if exp env cont)
  (eval (if-predicate exp) env
	(make-testc (if-consequent exp) (if-alternative exp) env cont)))

(define (eval-sequence exps env cont)
  (cond ((last-exp? exps) (eval (first-exp exps) env cont))
        (else (eval (first-exp exps) env
		    (make-beginc (rest-exps exps) env cont)))))

(define (eval-assignment exp env cont)
  (eval (assignment-value exp) env
	(make-assignc (assignment-variable exp) env cont)))

(define (eval-definition exp env cont)
  (eval (definition-value exp) env
	(make-definec (definition-variable exp) env cont)))

;;; Sec 4.1.2
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (or? exp) (tagged-list? exp 'or))
(define (or-choices exp) (cdr exp))

(define (or->if exp)
  (if (null? (or-choices exp)) 'false
      `(let ((_tmp_ ,(car (or-choices exp))))
	 (if _tmp_ _tmp_ (or ,@(cdr (or-choices exp)))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars bindings)
  (if (null? bindings) '()
      (cons (caar bindings) (let-vars (cdr bindings)))))
(define (let-vals bindings)
  (if (null? bindings) '()
      (cons (cadar bindings) (let-vals (cdr bindings)))))
(define (expand-let exp)
  `((lambda ,(let-vars (let-bindings exp))
      ,@(let-body exp))
    ,@(let-vals (let-bindings exp))))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;; Changes for CATCH-THROW begins
(define (catch? exp) (tagged-list? exp 'catch))
(define (catch-tag exp) (cadr exp))
(define (catch-body exp) (cddr exp))

(define (throw? exp) (tagged-list? exp 'throw))
(define (throw-tag exp) (cadr exp))
(define (throw-body exp) (caddr exp))
;;; Changes for CATCH-THROW ends

;;; data representation for continuations
(define (make-haltc) 'haltc)
(define (haltc? cont) (eq? cont 'haltc))

(define (make-testc true-exp false-exp env cont)
  (list 'testc true-exp false-exp env cont))
(define (testc? cont) (tagged-list? cont 'testc))
(define (testc-true cont) (cadr cont))
(define (testc-false cont) (caddr cont))
(define (testc-env cont) (cadddr cont))
(define (testc-cont cont) (car (cddddr cont)))

(define (make-beginc exps env cont)
  (list 'beginc exps env cont))
(define (beginc? cont) (tagged-list? cont 'beginc))
(define (beginc-rest-exps cont) (cadr cont))
(define (beginc-env cont) (caddr cont))
(define (beginc-cont cont) (cadddr cont))

(define (make-assignc var env cont) (list 'assignc var env cont))
(define (assignc? cont) (tagged-list? cont 'assignc))
(define (assignc-var cont) (cadr cont))
(define (assignc-env cont) (caddr cont))
(define (assignc-cont cont) (cadddr cont))

(define (make-definec var env cont) (list 'definec var env cont))
(define (definec? cont) (tagged-list? cont 'definec))
(define (definec-var cont) (cadr cont))
(define (definec-env cont) (caddr cont))
(define (definec-cont cont) (cadddr cont))

(define (make-operandsc exps env cont)
  (list 'operandsc exps env cont))
(define (operandsc? cont) (tagged-list? cont 'operandsc))
(define (operandsc-exps cont) (cadr cont))
(define (operandsc-env cont) (caddr cont))
(define (operandsc-cont cont) (cadddr cont))

(define (make-applyc proc cont)
  (list 'applyc proc cont))
(define (applyc? cont) (tagged-list? cont 'applyc))
(define (applyc-proc cont) (cadr cont))
(define (applyc-cont cont) (caddr cont))

(define (make-restopsc exps env cont)
  (list 'restopsc exps env cont))
(define (restopsc? cont) (tagged-list? cont 'restopsc))
(define (restopsc-rest cont) (cadr cont))
(define (restopsc-env cont) (caddr cont))
(define (restopsc-cont cont) (cadddr cont))

(define (make-consc val cont)
  (list 'consc val cont))
(define (consc? cont) (tagged-list? cont 'consc))
(define (consc-val cont) (cadr cont))
(define (consc-cont cont) (caddr cont))

;;; Changes for CATCH-THROW begins
(define (make-cabodyc body env cont) (list 'cabodyc body env cont))
(define (cabodyc? cont) (tagged-list? cont 'cabodyc))
(define (cabodyc-body cont) (cadr cont))
(define (cabodyc-env cont) (caddr cont))
(define (cabodyc-cont cont) (cadddr cont))

(define (make-catchc tag-handler cont) (list 'catchc tag-handler cont))
(define (catchc? cont) (tagged-list? cont 'catchc))
(define (catchc-tag cont) (car (cadr cont)))
(define (catchc-handler cont) (cdr (cadr cont)))
(define (catchc-cont cont) (caddr cont))

(define (make-thbodyc body env cont) (list 'thbodyc body env cont))
(define (thbodyc? cont) (tagged-list? cont 'thbodyc))
(define (thbodyc-body cont) (cadr cont))
(define (thbodyc-env cont) (caddr cont))
(define (thbodyc-cont cont) (cadddr cont))

(define (make-throwc tag cont) (list 'throwc tag cont))
(define (throwc? cont) (tagged-list? cont 'throwc))
(define (throwc-tag cont) (cadr cont))
(define (throwc-cont cont) (caddr cont))

(define (first-matching-catch thrown-tag cont)
  (define (loop cont)
    (cond ((haltc? cont) false)
	  ((testc? cont) (loop (testc-cont cont)))
	  ((assignc? cont) (loop (assignc-cont cont)))
	  ((definec? cont) (loop (definec-cont cont)))
	  ((beginc? cont) (loop (beginc-cont cont)))
	  ((operandsc? cont) (loop (operandsc-cont cont)))
	  ((applyc? cont) (loop (applyc-cont cont)))
	  ((restopsc? cont) (loop (restopsc-cont cont)))
	  ((consc? cont) (loop (consc-cont cont)))
	  ((cabodyc? cont) (loop (cabodyc-cont cont)))
	  ((catchc? cont) 
	   (if (eq? thrown-tag (catchc-tag cont))
	       (catchc-cont cont)
	       (loop (catchc-cont cont))))
	  ((thbodyc? cont) (loop (thbodyc-cont cont)))
	  ((throwc? cont) (loop (throwc-cont cont)))
	  (else (error "Unknown continuation type -- FIRST-MATCHING-CATCH" cont))))
  (loop cont))
;;; Changes for CATCH-THROW ends

;;; Sec. 4.1.3
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;; Sec. 4.1.4
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list 'zero? zero?)
	(list '> >)
	(list '< <)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
        (list 'eq? eq?)
        (list '= =)
;;        <more primitives>
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args cont)
  (apply-cont cont (apply
		    (primitive-implementation proc) args)))

(define input-prompt ";;; CPS-Eval input:")
(define output-prompt ";;; CPS-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let* ((input (read))
	 (output (eval input the-global-environment (make-haltc))))
    (cond ((tagged-list? output 'normal)
	   (announce-output output-prompt)
	   (user-print (cadr output)))
	  ((tagged-list? output 'uncaught)
	   (display ";;; Uncaught exception: ")
	   (display (cadr output))
	   (display " ")
	   (user-print (caddr output))))
    (driver-loop)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
