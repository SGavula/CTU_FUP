#lang racket
; Splitting matrix into blocks, code from: https://stackoverflow.com/questions/71594583/making-blocks-of-matrix-represented-by-list-in-racket
; (define splitted-list (split-list 2 '(0 0 75 75 75)))

(define mat1 '((0 0 75.0 75.0 75.0) 
              (180.0 180.0 225.0 225.0 225.0) 
              (180.0 180.0 225.0 225.0 225.0) 
              (180.0 180.0 225.0 225.0 225.0)))

#|
(define (split-mat n mat)
  (define (split-list n lst)
    (define (iter cout segment rest)
      (cond [(empty? rest) (list segment)]
            [(= 0 cout) (cons segment (iter (- n 1) (list (car rest)) (cdr rest)))]
            [else (iter (- cout 1) (append segment (list (car rest))) (cdr rest))])
      )

    (iter (- n 1) (list (car lst)) (cdr lst)))
  
  (map (lambda (x) (split-list n x)) mat))
; (define splitted-mat (map (lambda (x) (split-list 2 x)) '((0 0 75 75 75) (180 180 225 225 225))))

(define splitted-mat (split-mat 2 mat1))

(define (filter-mat n mat)
  (define (filter-lst lst)
    (filter (lambda (x) (= n (length x))) lst))
  (map (lambda (x) (filter-lst x)) mat))

(define filtered-mat (filter-mat 2 splitted-mat))

(define (calc-sum-mat n mat)
  (define (sum-lst lst)
    (map (lambda (x) (apply + x)) lst))
  (map (lambda (x) (sum-lst x)) mat))

(define sum-mat (calc-sum-mat 2 filtered-mat))

(define (average-mat n mat)
  (map (lambda (x) (map (lambda (y) (/ y 2)) x)) sum-mat))

(average-mat 2 sum-mat)
|#

(define (split-mat n mat)
  (define (split-list n lst)
    (define (iter cout segment rest)
      (cond [(empty? rest) (list segment)]
            [(= 0 cout) (cons segment (iter (- n 1) (list (car rest)) (cdr rest)))]
            [else (iter (- cout 1) (append segment (list (car rest))) (cdr rest))])
      )

    (iter (- n 1) (list (car lst)) (cdr lst)))
  
  (map (lambda (x) (split-list n x)) mat))

(define (filter-mat n mat)
  (define (filter-lst lst)
    (filter (lambda (x) (= n (length x))) lst))
  (map (lambda (x) (filter-lst x)) mat))

(define (calc-sum-mat n mat)
  (define (sum-lst lst)
    (map (lambda (x) (apply + x)) lst))
  (map (lambda (x) (sum-lst x)) mat))

(define (average-mat n mat)
  (map (lambda (x) (map (lambda (y) (/ y 2)) x)) mat))

(define (transpose mat)
  (apply map list mat))

(define (row-avg n mat)
  (let* [(splitted-mat (split-mat n mat))
         (filtered-mat (filter-mat n splitted-mat))
         (sum-mat (calc-sum-mat n filtered-mat))
         (averaged-mat (average-mat n sum-mat))]
    averaged-mat))


(define (mat->avg-mat w h mat)
  (let* [(row-avg-mat (row-avg w mat))
         (trsp-row-mat (transpose row-avg-mat))
         (col-avg-mat (row-avg h trsp-row-mat))
         (res-mat (transpose col-avg-mat))]
    row-avg-mat))

(row-avg 2 mat1)
; (define row-avg-mat (row-avg 2 mat1))
; (displayln "Averaged rows in mat: ")
; row-avg-mat

; (displayln "Transpose Averaged mat: ")
; (apply map list row-avg-mat)

; (displayln "Averaged mat: ")
; (transpose (row-avg 2 (transpose row-avg-mat)))