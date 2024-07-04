#lang racket

(displayln "Filter h-o-f")
; Filtering values with filter function
(filter (lambda (x) (not (= 1 x))) '(2 1 4 5 7 1 12))

(displayln "")
(displayln "Foldl h-o-f")
; Adding two lists, pay attention to syntax
(define (add-val-to-lst value lst)
  (cons value lst))

(add-val-to-lst '(1 2 3) 2)

(foldl add-val-to-lst '(1 5 5 2) '(2 5 4))

(displayln "")
(displayln "Map h-o-f")

; Map applies the lambda iterate through the list and take from from list to x parameter
(map (lambda (x) (+ x 10)) '(1 2 3))
; We can image it like this
#|
for(int i = 0; i < lst.length; i++):
    lst[i] = lst[i] + 10
|#

; This lines are equal
(map car '((a b c) (c d) (a d)))

(map (lambda (x) (car x)) '((a b c) (c d) (a d)))

; This lines are also equivalent
(map + '(1 2 3) '(1 2 3))

(map (lambda (x y) (+ x y)) '(1 2 3) '(1 2 3))

; Disadvantage of lamda notaion is that that when you want to add 3 vectors you must add another parameter to lambda
(map + '(1 2 3) '(1 2 3) '(2 2 5))

(map (lambda (x y z) (+ x y z)) '(1 2 3) '(1 2 3) '(2 2 5))

(map + '(1 2 3) '(1 2 3) '(2 2 5) '(2 2 5))

(map (lambda (x y z u) (+ x y z u)) '(1 2 3) '(1 2 3) '(2 2 5) '(2 2 5))

(displayln "")
(displayln "Apply h-o-f")
(apply map + '((1 2 3)
               (3 2 1)
               (-4 -4 -4)))
; I think that (apply map ...) do this
(map + '(1 2 3) '(3 2 1) '(-4 -4 -4))
