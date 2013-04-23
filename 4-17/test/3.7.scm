(use gauche.test)
(add-load-path ".")

(test-start "3.7")

(load "3.7")

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(test* "peter-accを使って40引出" 60 ((peter-acc 'open-sesame 'withdraw) 40))
(test* "さらにpaul-accを使って40引出" 20 ((paul-acc 'rosebud 'withdraw) 40))
(test* "さらにpaul-accを使って30振込" 50 ((paul-acc 'rosebud 'deposit) 30))
(test* "peter-accのパスワードを間違える" "Incorrect password" ((peter-acc 'some-other-password 'diposit) 50))
(test* "paul-accのパスワードを間違える" "Incorrect password" ((paul-acc 'some-other-password 'diposit) 50))
