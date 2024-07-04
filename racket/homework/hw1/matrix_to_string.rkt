#lang racket

(define chars " .,:;ox%#@")
(define mat '((90.0 150.0) (180.0 225.0)))

(define (val->idx val)
  (define d (string-length chars))
  (exact-round (floor (/ (* d (- 255 (floor val))) 256))))

; (val->idx 90.0)

(define (val-mat->str-mat mat)
  (define (val->str lst)
  (map (lambda (x) (list-ref (string->list chars) (val->idx x))) lst))
  
  (map val->str mat))

; (string-ref chars (val->idx x))
; (list-ref (string->list chars) (val->idx x))

; (define str-mat (val-mat->str-mat mat))

; (string-append (string-join (map list->string str-mat) "\n") "\n")

(define (mat->str mat)
  (let* [(str-mat (val-mat->str-mat mat))
         (str-mat-list (map list->string str-mat))
         (res (string-append (string-join str-mat-list "\n") "\n"))]
    res))

(mat->str mat)