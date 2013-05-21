(use gauche.test)
(add-load-path ".")

(test-start "3.25")

(load "3.25")

(define table (make-table))
((table 'insert!) '(食べ物 うどん 色) '白)
((table 'insert!) '(食べ物 うどん のどごし) 'つるつる)
((table 'insert!) '(食べ物 カニ 色) '赤)
((table 'insert!) '(食べ物 カニ 足の数) '8本)
(test* "うどんの色を取得" '白 ((table 'lookup) '(食べ物 うどん 色)))
(test* "うどんののどごしを取得" 'つるつる ((table 'lookup) '(食べ物 うどん のどごし)))
(test* "カニの色を取得" '赤 ((table 'lookup) '(食べ物 カニ 色)))
(test* "カニの足の数を取得" '8本 ((table 'lookup) '(食べ物 カニ 足の数)))
(test* "カニののどごしを取得" #f ((table 'lookup) '(食べ物 カニ のどごし)))