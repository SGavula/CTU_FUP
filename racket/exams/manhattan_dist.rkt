#lang racket
; (provide grid)

#|
(define (grid points)
    ; Implement me!
)
|#

(define points
 '((#\A 1 1)
   (#\B 1 6)
   (#\C 8 3)
   (#\D 3 4)
   (#\E 5 5)
   (#\F 8 9)))

(define (absValue a b)
  (abs (- a b)))

(define (manhattan point-x point-y)
  (apply + (map absValue point-x point-y)))

(define (get-max-x points)
  (apply max (map cadr points)))

(define (get-max-y points)
  (apply max (map caddr points)))

(define (test1 point points)
  (let* ([list-letters (map car points)]
         [list-y (map cadr points)]
         [list-x (map caddr points)]
         [distances (map (lambda (x y z) (list z (manhattan point (list x y)))) list-x list-y list-letters)]
         [sorted-dist (sort distances #:key cadr <)]
         [first-letter (car (car sorted-dist))]
         [first-dist (cadr (car sorted-dist))]
         [second-dist (cadr (cadr sorted-dist))])
   (cond [(= first-dist second-dist) #\.]
         [(= first-dist 0) first-letter]
         [(char-downcase first-letter)])))

(define (test points max-x max-y)
    (for*/list ([x (range (add1 max-y))]
                [y (range (add1 max-x))])
      (test1 (list x y) points)))

(define (split n k lst [acc '()])
  (cond
    [(empty? lst) (cons acc '())]
    [(= k 0) (cons (reverse acc) (split n n lst '()))]
    [(split n (- k 1) (cdr lst) (cons (car lst) acc))]))

(define (grid points)
  (let* ([max-x (get-max-x points)]
         [max-y (get-max-y points)]
         [letter-lst (test points max-x max-y)]
         [splitted-lst (split max-y max-y letter-lst)]
         [res (map list->string splitted-lst)])
    res))
