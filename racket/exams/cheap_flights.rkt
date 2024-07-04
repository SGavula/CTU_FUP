#lang racket
(define points
 '((#\A 1 1)
   (#\B 1 6)
   (#\C 8 3)
   (#\D 3 4)
   (#\E 5 5)
   (#\F 8 9)))

(define (absdiff a b) (abs (- a b)))

(define (manhattan point-x point-y)
  (apply + (map (lambda (x y) (absdiff x y)) point-x point-y)))