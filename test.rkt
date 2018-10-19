#lang s-exp "bidir-typeck.rkt"

;; Values
#t
0
"foo"

;; if expressions
(if #t "hello" "world")
(if (if #f #t #f) 0 1)

;; + expressions
+
(+ 1 1)
