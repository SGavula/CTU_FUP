#lang racket
; 1 (2 3) => ((1 2 3) (2 1 3) (2 3 1))
; (map (curry cons 3) '((1)))

(define (interleave el lst)
  (if (null? lst)
      ; there is only a single way one can insert el into '()
      (list (list el))                          
      ; otherwise one possibility is to prepend el to lst
      (cons (cons el lst)
            ; for the rest take all possible insertions of el into (cdr lst) 
            (map (curry cons (car lst))
                 ; and prepend (car lst) to each of them
                 (interleave el (cdr lst))))))

(interleave 1 '(2 3 4))

; (cons '(1 2 3) )
; (map (curry cons 2) (interleave 1 '(3)))
; (interleave 1 '(3))
; (cons '(1 3) (map (curry cons 3) (interleave 1 '())))

; (cons '(1 3) (map (curry cons 3) '((1))) )
; (cons '(1 3) '((3 1)))


; (map (curry cons 3) '((1)) ) = (cons 3 '(1))

; Takze my pridame element na zaciatok a potom

; '((1 2 3) (2 1 3) (2 3 1))
; '((1 3) (3 1))
; '((1))

; 1 '(3) -> '((1 3) (3 1))