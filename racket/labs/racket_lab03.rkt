#lang racket
(define lst1 '(1 2 3))
(define lst2 '(-2 0))
#|
(define lst1 '(-2 0))
(define lst2 '(1 2 3))
|#

; Different results

#|
(map (lambda (x) (* 1 x)) lst1)
(map (lambda (x) (* 2 x)) lst1)
(map (lambda (x) (* 3 x)) lst1)
|#

(define (mult-all-pairs lst1 lst2)
  (apply append (map (lambda (y) (map (lambda (x) (* y x)) lst2)) lst1))
)
; This is equivalent

(define (mult-all-pairs-vs-2 lst1 lst2)
  (apply append (map (lambda (x) (map ((curry *) x) lst2)) lst1))
 )

(mult-all-pairs lst1 lst2)
(mult-all-pairs-vs-2 lst1 lst2)

(define (f-all-pairs f lst1 lst2)
  (apply append (map (lambda (x) (map ((curry f) x) lst2)) lst1))
)

(f-all-pairs cons '(1 2 3) '(a b))


; Second task
(define (get-coef m) (car m)) ; first component
(define (get-exp m) (cadr m)) ; second component

; ((1 0) (-1 0)) -> ((0 0))
; ((2 2) (3 2)) -> ((5 2))
(define (add-mon m1 m2)
  (list (+ (get-coef m1) (get-coef m2)) (get-exp m1)))

; ((1 1) (1 2)) -> (1 3)
(define (mult-mon m1 m2)
  (list (* (get-coef m1) (get-coef m2)) (+ (get-exp m1) (get-exp m2))))

(mult-mon '(1 1) '(1 2))

(define (add-mon-pol mon pol)
  (displayln mon)
  (displayln pol)
  (define (same-exp? m) (= (get-exp m) (get-exp mon)))
  (define same-mon (filter same-exp? pol))
  (define rest (filter (compose not same-exp?) pol))
  (displayln "Same monoid")
  (displayln same-mon)
  (displayln "Rest")
  (displayln rest)
  (displayln "")

  (if (empty? same-mon)
      (cons mon pol)
      (cons (add-mon (car same-mon) mon) rest)))

;(add-mon-pol '(1 2) '((0 0) (2 1) (3 2)))

(foldl add-mon-pol '((-1 0) (1 1) (3 2)) '((1 0) (1 1)))

; '(0 2) '((0 0) (2 1) (3 2)) -> '(3 2) 
#|
(define (my-add-mon-pol mon pol)
  (filter (lambda (x) (eq? (get-exp mon) (get-exp x))) pol))

(my-add-mon-pol '(1 2) '((0 0) (2 1) (3 2)))
|#

(define (add-val-to-lst lst value)
  (cons value lst))

(add-val-to-lst '(1 2 3) 2)

(foldl add-val-to-lst '(2 5 4) '((1 5 5 2)))


; Task 1
(define (linear-combination matrix coef-vec)
  (displayln "Coef vector:")
  (displayln matrix)
  (displayln "")
  (define (mul-vec-coef vec)
    (foldl + 0 (map * vec coef-vec)))
  
  (map mul-vec-coef (apply map list matrix)))

(linear-combination '((1 2 3) (1 0 1) (0 2 0)) '(2 -1 3))

; Task 2

(define (matrix-mult m1 m2)
  (define (linear-combination coef-vec)
  (displayln "Coef vector:")
  (displayln m2)
  (displayln "")
  (define (mul-vec-coef vec)
    (foldl + 0 (map * vec coef-vec)))
  
  (map mul-vec-coef (apply map list m2)))

  (map linear-combination m1))

(matrix-mult '((1 2 3) 
               (-1 0 2)) 
              '((1 -1) 
                (2 0) 
                (0 3))) 