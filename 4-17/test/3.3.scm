(use gauche.test)
(add-load-path ".")

(test-start "3.3")

(load "3.3")

(test-section "Account test")
(define acc (make-account 100 'secret-password))
(test* "((acc 'secret-password 'withdraw) 30)" 70 ((acc 'secret-password 'withdraw) 30))
(test* "((acc 'secret-password 'diposit) 40)" 110 ((acc 'secret-password 'diposit) 40))
(test* "((acc 'secret-password 'withdraw) 300)" *test-error* ((acc 'secret-password 'withdraw) 300))
(test* "((acc 'secret-password 'mes) 10)" *test-error* ((acc 'secret-password 'mes) 10))

(test-section "Password test")
(define acc (make-account 100 'secret-password))
(test* "((acc 'secret-password 'withdraw) 40)" 60 ((acc 'secret-password 'withdraw) 40))
(test* "((acc 'some-other-password 'diposit) 50)" "Incorrect password" ((acc 'some-other-password 'diposit) 50))
