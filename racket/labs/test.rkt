#lang racket
(define (permutate el lst)
  (if (empty? lst)
      (list(list el))
      (cons (cons el lst) (map (curry cons (car lst)) (permutate el (cdr lst))))))

(permutate 1 '(2 3))

(define (permutations lst)
  (if (empty? lst)
      '(())
      (apply append (map (lambda (x) (permutate (car lst) x)) (permutations (cdr lst))) )))

(permutations '(1 2 3))