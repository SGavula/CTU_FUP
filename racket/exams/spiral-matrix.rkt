#lang racket

(define mat '((1 2 3)
              (8 9 4)
              (7 6 5)))

(define (add-n-mat mat n)
  (map (lambda (x) (map (lambda (y) (+ y n)) x)) mat))

(define (wrap x mat y)
  ; (display y))
  (cons x (append mat (list y))))

(define (add-horizontally curr-mat n)
  (let* ([first-col (range (- (* 4 n ) 4) (- (* 3 n) 2) -1)]
         [last-col (range (+ n 1) (- (* 2 n) 1))])
    (map wrap first-col curr-mat last-col)))

(define (add-vertically curr-mat n)
  (let* ([first (range 1 (+ n 1))]
         [last (range (- (* 3 n) 2) (- (* 2 n) 2) -1)])
    (wrap first curr-mat last)))

(define (spiral-matrix n)
  (cond [(equal? n 1) '((1))]
        [(let* ([curr-mat (spiral-matrix (- n 2))]
                [add-n-mat (add-n-mat curr-mat (- (* 4 n) 4))]
                [hor-curr-mat (add-horizontally add-n-mat n)]
                [add-vertically (add-vertically hor-curr-mat n)])
           add-vertically)]))