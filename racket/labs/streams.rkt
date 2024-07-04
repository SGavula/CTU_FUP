#lang racket
; (stream-cons #<stream1> #<stream2>)
; (stream-first stream) -> #<stream1>
; (stream-rest stream) -> #<stream2>

; Definition of streams, using stream-cons
(define ones (stream-cons 1 ones))
(define twos (stream-cons 2 twos))

(define (num-stream n)
    (stream-cons n (num-stream (+ n 1))))

; Adding two streams together
(define ones-twos (stream-cons ones twos))

; Getting elements of stream, it prints like first stream of ones and then it prints 10 elements from the second stream
(stream->list (stream-take ones-twos 10)) ; it gives us -> '(#<stream> 2 2 2 2 2 2 2 2 2)

; printing 10 elements from just first stream
(stream->list (stream-take (stream-first ones-twos) 10)) ; it gives us -> '(1 1 1 1 1 1 1 1 1 1)

(stream->list (stream-take (stream-rest ones-twos) 10)) ; it gives us -> '(2 2 2 2 2 2 2 2 2 2)3

(stream->list (stream-take (num-stream 0) 10))

; adding two numbers of streams
(+ (stream-first ones) (stream-first twos)) ; equivalent to (+ 1 2) = 3

; adding two streams
(define (add-streams s1 s2)
  (stream-cons
   (+ (stream-first s1) (stream-first s2))
   (add-streams (stream-rest s1) (stream-rest s2))))

(stream->list (stream-take (add-streams (num-stream 0) ones) 5)) ; it gives -> '(1 2 3 4 5)
(stream->list (stream-take (add-streams (num-stream 1) ones) 5)) ; it gives -> '(2 3 4 5 6)