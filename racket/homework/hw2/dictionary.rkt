#lang racket

;; Define a global parameter to hold the constants
(define constants-param
  (make-parameter (make-hash)))

;; Function to add constants to the global dictionary
(define (add-constant name value)
  (hash-set! (constants-param) name value))

;; Function to retrieve the value of a constant from the global dictionary
(define (get-constant name)
  (hash-ref (constants-param) name #f))

;; Example usage:
(add-constant 'STYLE "fill:red;opacity:0.2;stroke:red;stroke-width:3")
(add-constant 'START 195)
(add-constant 'END 10)

;; Retrieve constant values
(get-constant 'STYLE)
(get-constant 'START)
(get-constant 'END) 