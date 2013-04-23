(use gauche.test)
(add-load-path ".")

(test-start "3.3")

(load "3.3")

(test-section "Account test")
(define acc (make-account 100 'secret-password))
(test* "30引出" 70 ((acc 'secret-password 'withdraw) 30))
(test* "40振込" 110 ((acc 'secret-password 'deposit) 40))
(test* "300引出" "Insufficient funds" ((acc 'secret-password 'withdraw) 300))
(test* "存在しない操作をする" *test-error* ((acc 'secret-password 'mes) 10))

(test-section "Password test")
(define acc (make-account 100 'secret-password))
(test* "正しいパスワードで40引出" 60 ((acc 'secret-password 'withdraw) 40))
(test* "間違ったパスワードで50振込" "Incorrect password" ((acc 'some-other-password 'deposit) 50))
