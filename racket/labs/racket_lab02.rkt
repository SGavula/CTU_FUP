#lang racket

(define lst '(a b c d))

(define (reverse lst [acc '()])
  (if (empty? lst) acc
      (reverse (cdr lst) (cons (car lst) acc))))


(require racket/trace)
(trace reverse)

(define (group-same lst)
  (define (iter l gr)
    (cond [(empty? l) (list gr)]
          [(equal? (car gr) (car l))
           (iter (cdr l) (cons (car l) gr))]
          [else
           (cons gr (iter (cdr l) (list (car l))))]))
  (if (empty? lst)
      '()
      (iter (cdr lst) (list (car lst)))))

; Function above is the same as function below

#|
(define (iter l gr)
    (cond [(empty? l) (list gr)]
          [(equal? (car gr) (car l))
           (iter (cdr l) (cons (car l) gr))]
          [else
           (cons gr (iter (cdr l) (list (car l))))]))

(define (group-same lst)
  (if (empty? lst)
      '()
      (iter (cdr lst) (list (car lst)))))
|#

#|
(define (group-same lst)
  (define (iter l gr)
    (cond
      [(null? l) (list gr)]
      [(eqv? (car gr) (car l)) (iter (cdr l) (cons (car gr) gr))]
      [else (cons gr (iter (cdr l) (list (car l))))]))
  (if (null? lst)
      '()
      (iter (cdr lst) (list (car lst)))))
|#

(define (join-lengths grs)
  (map (lambda (g) (cons (car g) (length g))) grs))

(define (letter-frequencies str)
  (sort
   (join-lengths
    (group-same
     (sort
      (filter char-alphabetic? (string->list (string-downcase str)))
      char<?)))
   >
   #:key cdr))