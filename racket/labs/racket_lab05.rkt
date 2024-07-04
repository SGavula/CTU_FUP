#lang racket
; Lab exercise 01

(define (stream-add s1 s2)
  (stream-cons (+ (stream-first s1) (stream-first s2))
               (stream-add (stream-rest s1) (stream-rest s2))))

(define fib-stream
  (stream-cons 0
               (stream-cons 1
                            (stream-add fib-stream (stream-rest fib-stream)))))

(define fib-stream2
  (stream-cons 20 (stream-cons 5 (stream-cons 1 (stream-add fib-stream2 (stream-rest fib-stream2))))))