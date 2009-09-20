#!r6rs

(import (except (rnrs) read)
        (manued)
        (xunit))

(define-syntax assert-manued
  (syntax-rules ()
    ((_ str tree before after)
     (let ((datum (string->datum str)))
       (assert-equal? tree datum)
       (call-with-values
           (lambda () (datum->before&after datum))
         (lambda (b a)
           (assert-equal? before b)
           (assert-equal? after a)))))))

(assert-manued "" '() "" "")
(assert-manued "manued" '(#\m #\a #\n #\u #\e #\d) "manued" "manued")
(assert-manued "[a/b]" '((sub (#\a) (#\b))) "a" "b")
(assert-manued "[a||b]" '((swap (#\a) () (#\b))) "ab" "ba")
(assert-manued "[a|b|c]" '((swap (#\a) (#\b) (#\c))) "abc" "cba")
(assert-manued "[a~//b~]]" '((sub (#\a #\/) (#\b #\]))) "a/" "b]")
(assert-manued "[a||b]" '((swap (#\a) () (#\b))) "ab" "ba")
(assert-manued "ma[u||n]ed" '(#\m #\a (swap (#\u) () (#\n)) #\e #\d) "mauned" "manued")
(assert-manued "あ[あ/]い[か|お|[う/]うえ][/き]くけ[/こ]"
               '(#\あ
                 (sub (#\あ) ())
                 #\い
                 (swap (#\か) (#\お) ((sub (#\う) ()) #\う #\え))
                 (sub () (#\き))
                 #\く
                 #\け
                 (sub () (#\こ)))
               "ああいかおううえくけ"
               "あいうえおかきくけこ")

(report)
