#lang racket

(define (patt-match lst)
  (match lst
    [(list 'define  body)]
    [(list cmd () ...) (format "Cmd: ~a | Rest: ~a" cmd rest)]
    [(list) 'empty]
    [(list x) (format "singleton (~a)" x)]
    [(list 'fn ys ...) (format "fn and rest ~a" ys)]
    [(list (list 'fn args ...) ys ...) (format "fn with ~a and1 rest ~a" args ys)]
    [(list 1 2 ys ... z) (format "1, 2, rest ~a and last ~a" ys z)]
    [_ 'other]))