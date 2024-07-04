#lang racket

; SVG Generating Functions
(define (circle x y r style)
  (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" x y r style))

(define (rect x y width height style)
  (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x y width height style))

(define (line x1 y1 x2 y2 style)
  (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1 y1 x2 y2 style))

; Example usage:
(displayln (circle 50 40 20 "fill:blue"))
(displayln (rect 10 20 30 40 "fill:blue"))
(displayln (line 10 20 30 40 "stroke:black;stroke-width:5"))

(define test2
  '((define STYLE "fill:red;opacity:0.2;stroke:red;stroke-width:3")
    (define START 195)
    (define END 10)
    (define (circles x r)
      (when (> r END)
        (circle x 200 r STYLE)
        (circles (+ x (floor (/ r 2))) (floor (/ r 2)))))))

#|
(define (is-func? line)
  (cond
    [(list? (car (cdr line))) #t]
    [else #f]))

(define (execute-func prg)
  )

(define (execute-line width height prg expr)
  (if (is-func? (car prg)) (displayln (cdr (car prg))) (execute width height (cdr prg) expr)))

; (car prg) (execute width height (cdr prg) expr)

(define (execute width height prg expr)
  (cond [(empty? prg) (displayln "Vrat ten strign svg and finish function")]
        [else (execute-line width height prg expr)]))

(execute 400 400 test2 '(circles 40 5))





(is-func? '(define (circles x r)
      (when (> r END)
        (circle x 200 r STYLE)
        (circles (+ x (floor (/ r 2))) (floor (/ r 2))))))

(is-func? '(define STYLE "fill:red;opacity:0.2;stroke:red;stroke-width:3"))
|#