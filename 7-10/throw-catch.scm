;; throw-catch from http://d.hatena.ne.jp/yagiey/20100505/1273059570
;;捕捉した継続のための大域変数
(define *signals* '())

;;try、catch、finallyに相当するところ
(define-syntax catch
  (syntax-rules (finally)
    ((_ (sig body ...) (finally follow ...))
     (let* ((signals-backup *signals*)
            (val (call/cc (lambda (k)
                            (set! *signals* (cons (cons 'sig k) *signals*))
                            body ...))))
       (set! *signals* signals-backup)
       follow ...
       val))
    ((_ sig body ...)
     (catch (sig body ...) (finally)))))

;;throwに相当するところ
(define-syntax throw
  (syntax-rules ()
    ((_ sig val)
     ((cdr (assq 'sig *signals*)) val))))

