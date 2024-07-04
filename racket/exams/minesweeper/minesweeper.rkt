#lang racket
; for testing
(define test-board
  (map string->list (string-split ".*..\n..*.\n**..\n...*\n*...")))

(define test-board2
  (map string->list (string-split ".*.*.\n..*..\n..*..\n.....")))

; for converting ints to chars.
(define (int->digit i) (integer->char (+ 48 i)))

; (let* ((input-string (port->lines))
        ; implement parsing of board/sweep for mines 
        ; assuming counted-board contins a list of list of chars
;       (sn (map list->string counted-board)))
;  (for ((l sn))
;    (display l)
;    (newline)))

; (define get-all-neigh x y )

(define (get-square board x y)
  (list-ref (list-ref board x) y))

(define (get-neighbours board mx my x y)
  (for*/list ([i (range (max 0 (- x 1)) (min mx (+ x 2)))]
              [j (range (max 0 (- y 1)) (min my (+ y 2)))])
    (list-ref (list-ref board i) j)))

(define (count-mines lst)
  (let* ([filter-mines (filter (curry equal? #\*) lst)]
         [num_mines (length filter-mines)])
    (if (zero? num_mines) #\. num_mines)))

(define (get-num-mines board mx my x y)
  (cond
    [(equal? (get-square board x y) #\*) #\*]
    [(let* ([neighbours (get-neighbours board mx my x y)]
         [num-mines (count-mines neighbours)])
    num-mines)]))

(define (get-num-board board)
  (define rows (length board))
  (define columns (length (first board)))
  (for*/list ([i (range rows)]
              [j (range columns)])
    (get-num-mines board rows columns i j)))

(define (num-board->char-board num-board)
  (map (lambda (x) (if (char? x) x (int->digit x))) num-board))

(define (group lst n count [acc '()])
  (cond [(empty? lst) (cons (reverse acc) '())]
        [(zero? count) (cons (reverse acc) (group lst n n '()))]
        [(group (cdr lst) n (- count 1) (cons (car lst) acc))]))

(define (str-lst->str lst acc)
  (cond [(empty? lst) acc]
        [(str-lst->str (cdr lst) (string-append acc (string-append (car lst) "\n")))]))

(define (minesweeper board)
  (define columns (length (car board)))
  (let* ([num-board (get-num-board board)]
         [char-board (num-board->char-board num-board)]
         [grupped-board (group char-board columns columns)]
         [grupped-board-str (map list->string grupped-board)]
         [res (str-lst->str grupped-board-str "")])
    res))