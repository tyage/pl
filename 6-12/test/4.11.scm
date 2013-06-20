(use gauche.test)
(add-load-path ".")

(test-start "4.11")

(load "4.11")

(define frame (make-frame '(key0) '(0)))
(define env (list frame))

(test-section "frame test")
(test* "(frame-variables frame)" (frame-variables frame) '(key0))
(test* "(frame-values frame)" (frame-values frame) '(0))
(add-binding-to-frame! 'key1 1 frame)
(add-binding-to-frame! 'key2 2 frame)
(test* "(frame-variables frame)" (frame-variables frame) '(key0 key1 key2))
(test* "(frame-values frame)" (frame-values frame) '(0 1 2))

(test-section "env test")
(test* "(lookup-variable-value 'key0 env)" (lookup-variable-value 'key0 env) 0)
(test* "(lookup-variable-value 'key1 env)" (lookup-variable-value 'key1 env) 1)
(set-variable-value! 'key2 3 env)
(test* "(lookup-variable-value 'key2 env)" 
	(lookup-variable-value 'key2 env) 3)
(define-variable! 'key3 4 env)
(test* "(lookup-variable-value 'key3 env)" 
	(lookup-variable-value 'key3 env) 4)

