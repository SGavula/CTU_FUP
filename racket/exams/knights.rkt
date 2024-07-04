#lang racket
(define valid-board '((1 0 0 0)
                      (0 0 0 1)
                      (1 0 0 0)
                      (0 1 0 0)))

(define invalid-board '((0 1 0 0)
                        (0 0 0 1)
                        (1 0 0 0)
                        (0 0 1 0)))

(define (process-square square row-id col-id)
  (if (= square 1) (list row-id col-id) '()))

(define (process-row row row-id col-id acc)
  (if (empty? row)
      (reverse acc)
     (let* ([coord (process-square (car row) row-id col-id)])
       (if (empty? coord) (process-row (cdr row) row-id (+ 1 col-id) acc) (process-row (cdr row) row-id (+ 1 col-id) (cons coord acc))))))

(define (process-board board row-id acc)
  (if (empty? board) acc
      (let* ([coords (process-row (car board) row-id 0 '())])
        (process-board (cdr board) (+ 1 row-id) (append acc coords)))))

(define (check-coords coord1 coord2)
  (let* ([first-diff (abs (- (car coord1) (car coord2)))]
         [second-diff (abs (- (cadr coord1) (cadr coord2)))]
         [fst-eq-one (= first-diff 1)]
         [snd-eq-one (= second-diff 1)]
         [fst-eq-two (= first-diff 2)]
         [snd-eq-two (= second-diff 2)]
         [fst-cond (and fst-eq-one snd-eq-two)]
         [snd-cond (and fst-eq-two snd-eq-one)]
         [fin-cond (or fst-cond snd-cond)])
    fin-cond))

(define (check-coord-all coord all-coords)
  (if (empty? all-coords) #f
      (let* ([check (check-coords coord (car all-coords))])
        (if check check (check-coord-all coord (cdr all-coords))))))

(define (check-all-coord-all all-coord all-coords)
  (if (empty? all-coord) #f
      (let* ([check (check-coord-all (car all-coord) all-coords)])
        (if check check (check-all-coord-all (cdr all-coord) all-coords)))))

(define (is_valid? board)
  (let* ([coords (process-board board 0 '())]
         [res (check-all-coord-all coords coords)])
    (if res #f #t)))