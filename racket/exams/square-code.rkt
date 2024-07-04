#lang racket
(provide encode)

(define test1 "If man was meant to stay on the ground, god would have given us roots.")
(define test2 "Have a nice day!")

(define (group-helper lst n k acc)
  (cond [(empty? lst) (cons (reverse acc) '())]
        [(equal? k 0) (cons (reverse acc) (group-helper lst n n '()))]
        [(group-helper (cdr lst) n (- k 1) (cons (car lst) acc))]))

(define (group lst c)
  (group-helper lst c c '()))

(define (process-to-transpose lst c)
  (define len (length lst))
  (if (equal? len c) lst (append lst (make-list (- c len) #\space))))

(define (add-spaces lst c)
  (define len (length lst))
  (if (equal? len c) (append lst '(#\space)) (append lst (make-list (- c len) #\space))))

(define (encode str)
  (let* ([str-downcase (string-downcase str)]
         [lst (filter char-alphabetic? (string->list str-downcase))]
         [c (exact-ceiling (sqrt (length lst)))]
         [groupped-text (group lst c)]
         [for-transpose (map (lambda (x) (process-to-transpose x c)) groupped-text)]
         [transpose (apply map list for-transpose)]
         [filtered-lst (map (lambda (x) (filter char-alphabetic? x)) transpose)]
         [lst-with-space (map (lambda (x) (add-spaces x c)) filtered-lst)]
         [char-lst (apply append lst-with-space)]
         [char-lst (apply append lst-with-space)]
         [without-last-space (take char-lst (- (length char-lst) 1))]
         [final-str (list->string without-last-space)])
    final-str))