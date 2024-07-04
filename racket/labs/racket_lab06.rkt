#lang racket
(struct tape (left val right) #:transparent)

(define t (tape '(3 2 1) 4 '(5 6 7)))

(define (change op t)
  (define new-val (op (tape-val t) 1))
  (tape (tape-left t) new-val (tape-right t)))

(define (move dir t)
  (cond
    [(eq? dir 'left) (tape (cdr (tape-left t)) (car (tape-left t)) (cons (tape-val t) (tape-right t)))]
    [(eq? dir 'right) (tape (cons (tape-val t) (tape-left t)) (car (tape-right t)) (cdr (tape-right t)))]))