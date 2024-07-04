#lang racket
(provide convex-hull)

(define points '((-2 3) (2 2) (-1 1) (-2 -1.5) (4 -1) (1 -3)))

(define (compare point-a point-b)
  (let* ([x1 (car point-a)]
         [y1 (cadr point-a)]
         [x2 (car point-b)]
         [y2 (cadr point-b)])
    (cond [(equal? y1 y2) (if (> x1 x2) point-a point-b)]
           [(if (< y1 y2) point-a point-b)])))

(define (initial_point points)
  (foldl compare (car points) points))

(define (get-angle point-a point-b)
  (let* ([x1 (car point-a)]
         [y1 (cadr point-a)]
         [x2 (car point-b)]
         [y2 (cadr point-b)]
         [angle (atan (- y2 y1) (- x2 x1))])
    (if (< angle 0) (+ angle (* 2 pi)) angle)))

(define (sort-angle p0 points)
  (define (xcomp a b)
    (if (< (get-angle p0 a) (get-angle p0 b)) #t #f))
  (let* ([filtered-points (filter (lambda (x) (not (equal? x p0))) points)]
         [sorted-points (sort filtered-points xcomp)])
    (cons p0 sorted-points)))

(define (is-left a b c)
  (let* ([x1 (car a)]
         [y1 (cadr a)]
         [x2 (car b)]
         [y2 (cadr b)]
         [x3 (car c)]
         [y3 (cadr c)]
         [left-turn-first (* (- x2 x1) (- y3 y1))]
         [left-turn-second (* (- y2 y1) (- x3 x1))]
         [left-turn (- left-turn-first left-turn-second)])
    (if (> left-turn 0) #\t #\f)))


(define (process-points sorted-points stack)
  (cond [(empty? sorted-points) (reverse stack)]
        [(< (length stack) 2) (process-points (cdr sorted-points) (cons (car sorted-points) stack))]
        [(let* ([p2 (car sorted-points)]
                [p1 (car stack)]
                [p0 (cadr stack)]
                [left-turn (is-left p0 p1 p2)])
           (if (equal? left-turn #\t)
               (process-points (cdr sorted-points) (cons p2 stack))
               (process-points sorted-points (cdr stack))))]))

(define (convex-hull points)
  (let* ([p0 (initial_point points)]
         [sorted-points (sort-angle p0 points)])
    (process-points sorted-points '())))