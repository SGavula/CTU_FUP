#lang racket

(require 2htdp/image)

(provide img->mat ascii-art)

(define example 
  (above
    (beside (rectangle 20 10 "solid" (make-color 0 0 0))
            (rectangle 30 10 "solid" (make-color 75 75 75)))
    (beside (rectangle 20 30 "solid" (make-color 180 180 180))
            (rectangle 30 30 "solid" (make-color 225 225 225)))))

(define (RGB->grayscale color)
  (+ (* 0.3 (color-red color))
     (* 0.59 (color-green color))
     (* 0.11 (color-blue color))))

(define (img->mat img)
  (define (create-mat width i lst [acc '()])
    (cond [(empty? lst) (list (reverse acc))]
          [(= i 0) (cons (reverse acc) (create-mat width (- width 1) (cdr lst) (list (car lst))))]
          [else (create-mat width (- i 1) (cdr lst) (cons (car lst) acc))]
          ))

  (create-mat (image-width img) (image-width img) (map RGB->grayscale (image->color-list img)))
)

; Code for spliting matrix into blocks
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
  (map (lambda (x) (map (lambda (y) (/ y n)) x)) mat))

(define (transpose mat)
  (apply map list mat))

(define (row-avg n mat)
  (let* [(splited-mat (split-mat n mat))
         (filtered-mat (filter-mat n splited-mat))
         (sum-mat (calc-sum-mat n filtered-mat))
         (averaged-mat (average-mat n sum-mat))]
    averaged-mat))


(define (mat->avg-mat w h mat)
  (let* [(row-avg-mat (row-avg w mat))
         (trsp-row-mat (transpose row-avg-mat))
         (col-avg-mat (row-avg h trsp-row-mat))
         (res-mat (transpose col-avg-mat))]
    res-mat))

; Code for converting values in matrix to string
(define (val->idx val chars)
  (define d (string-length chars))
  (exact-round (floor (/ (* d (- 255 (floor val))) 256))))

(define (val-mat->str-mat mat chars)
  (define (val->str lst)
  (map (lambda (x) (list-ref (string->list chars) (val->idx x chars))) lst))
  
  (map val->str mat))

(define (mat->str mat chars)
  (let* [(str-mat (val-mat->str-mat mat chars))
         (str-mat-list (map list->string str-mat))
         (res (string-append (string-join str-mat-list "\n") "\n"))]
    (if (equal? res "\n") "" res)))

(define (ascii-art width height chars)
  (lambda (image)
    (let* [(source-mat (img->mat image))
           (avg-mat (mat->avg-mat width height source-mat))
           (res-string (mat->str avg-mat chars))]
      res-string)))
