(use gauche.test)
(add-load-path ".")

(test-start "3.22")

(load "3.22")

(define q (make-queue))
(test* "front queue" *test-error* ((q 'front-queue)))
(test* "aを追加" '(a) ((q 'insert-queue!) 'a))
(test* "front queue" 'a ((q 'front-queue)))
(test* "bを追加" '(a b) ((q 'insert-queue!) 'b))
(test* "queueを削除" '(b) ((q 'delete-queue!)))
(test* "dを追加" '(b c) ((q 'insert-queue!) 'c))
(test* "queueを削除" '(c) ((q 'delete-queue!)))
(test* "queueを削除" '() ((q 'delete-queue!)))
(test* "queueを削除" *test-error* ((q 'delete-queue!)))
(test* "存在しない操作をする" *test-error* (q 'some-other-message))