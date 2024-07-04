#lang racket

; Lab task 01 a
(define (average-lst lst [acc 0] [sum 0])
  (if (empty? lst)
      (/ acc sum)
      (average-lst (cdr lst) (+ (car lst) acc) (+ 1 sum))))

#|
(require racket/trace)
(trace average-lst)
|#

(average-lst '(1 2 3))
(average-lst '(1 5 55 5 3))

; Lab task 01 b
(define (sum lst [acc 0])
  (if (empty? lst) acc
      (sum (cdr lst) (+ (car lst) acc))))

(define (avg-lst lst)
  (/ (sum lst) (length lst)))

(avg-lst '(1 5 55 5 3))

(define (group-same lst)
  (define (group chars chars-left)
    (cond [(empty? chars-left) (list chars)]
          [(eqv? (car chars) (car chars-left)) (group (cons (car chars-left) chars) (cdr chars-left))]
          [else (cons chars (group (list (car chars-left)) (cdr chars-left)))]))
  
  (if (empty? lst)
      '()
      (group (list (car lst)) (cdr lst))))

(require racket/trace)
(trace group-same)

(group-same '(#\c #\c #\z))

#|
(define (join-lengths lst [acc '()])
  (if (empty? lst)
      acc
      (join-lengths (cdr lst) (cons (cons (car (car lst)) (length (car lst))) acc)) ))
|#
(define (join-lengths lst)
  (map (lambda (inner) (cons (car inner) (length inner))) lst))

(join-lengths (list '(1 1 1 1) '(3 3 3 3)))

; (join-lengths ((#\c #\c) (#\z))) -> ((#\c . 2) (#\d . 1))

#|
(define (letter-frequencies str)
  (sort
   (join-lengths
   (group-same
    (sort
     (filter char-alphabetic?
             (string->list
              (string-downcase str)
     ))
     char<?))
   )
   > #:key cdr))
|#

; Local definitions

#|
(define (letter-frequencies str)
  (let* ([(downcase (string-downcase str))
        (string-list (string->list downcase))
        (filtered (filter char-alphabetic?))
        (sorted (sort filtered char<?))
        (grouped (group-same sorted))
        (joined (join-lengths grouped))
        (sorted-again (sort joined > #:key cdr))
        sorted-agian])))
|#

(define (letter-frequencies str)
  (let* ([downcase (string-downcase str)]
        [string-list (string->list downcase)]
        [filtered (filter char-alphabetic? string-list)]
        [sorted (sort filtered char<?)]
        [grouped (group-same sorted)]
        [joined (join-lengths grouped)]
        [sorted-again (sort joined > #:key cdr)])
         sorted-again))

(letter-frequencies "good")


#|
(define (letter-frequencies str)
  (let ([downcase (string-downcase str)]
        [string-list (string->list downcase)])
        string-list))

(letter-frequencies "good")
|#


(define x 10)
(define y (* x x))
(displayln y)

(let* ([x 5]
       [y (* x x)])
  y)

(let ([x 5]
      [y 10]
      [z 50])
  (+ x y z))

(define (split n i lst g)
  (cond [(empty? lst) (list g)]
        [(= i 0) (cons (reverse g) (split n (- n 1) (cdr lst) (list (car lst))))]
        [else (split n (- i 1) (cdr lst) (cons (car lst) g))]))

(define (split-list n lst)
  (if (empty? lst) '()
      (split n (- n 1) (cdr lst) (list (car lst)))))

(split-list 4 '(1 2 a b 1 2 3 4 5 6))
(split-list 3 '(a b 1 2))


; TASK 02
(define (cal-avg-lists lst [acc '()])
  (if (empty? lst) acc
      (cal-avg-lists (cdr lst) (cons (avg-lst (car lst)) acc))))

(define (n-block-average n lst)
  (let* ([lst-of-lst (split-list n lst)]
         [avg-lists (cal-avg-lists lst-of-lst)]
         [reversed-avg-lists (reverse avg-lists)])
    reversed-avg-lists))

(displayln "N BLOCKS AERAGE: ")
(n-block-average 3 '(1 3 1 5))