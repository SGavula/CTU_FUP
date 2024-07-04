#lang racket

; Define the environment structure to hold variable and function definitions
(define env (make-hash))
(define svg-markup "")

; Environment function
(define (circle x y r style local-env)
  (define x-val (arg->num x local-env))
  (define y-val (arg->num y local-env))
  (define r-val (arg->num r local-env))
  (define style-val (arg->num style local-env))
  (format "\n<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" x-val y-val r-val style-val))

(define (rect x y width height style local-env)
  ; (displayln x)
  ; (displayln y)
  ; (displayln width)
  ; (displayln height)
  ; (displayln style)
  (define x-val (arg->num x local-env))
  (define y-val (arg->num y local-env))
  (define width-val (arg->num width local-env))
  (define height-val (arg->num height local-env))
  (define style-val (arg->num style local-env))
  (format "\n<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x y width height style))

(define (line x1 y1 x2 y2 style)
  (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1 y1 x2 y2 style))

; A helper to add definitions to the environment
(define (define-var name value)
  (hash-set! env name value))

(define (sym->num sym [local-env (make-hash)])
  ; (displayln "Sym->num")
  (define str-sym (symbol->string sym))
  (cond
    [(string=? (string-upcase str-sym) str-sym) (hash-ref env sym)]
    [(string=? (string-downcase str-sym) str-sym) (hash-ref local-env sym)]))

(define (arg->num arg [local-env (make-hash)])
  ; (displayln "Arg->num")
  ; (for ([key (hash-keys local-env)])   (printf "Key: ~a, Value: ~a\n" key (hash-ref local-env key)))
  (cond
    [(list? arg) (eval-num-exp arg local-env)]
    [(number? arg) arg]
    [(string? arg) arg]
    [(symbol? arg) (sym->num arg local-env)]
    [else (error "Unknown argument")]))

; Evaluate numeric expressions
(define (eval-num-exp exp [local-env (make-hash)])
  ; (displayln "eval-num-exp")
  ; (for ([key (hash-keys local-env)])   (printf "Key: ~a, Value: ~a\n" key (hash-ref local-env key)))
  (cond
    [(number? exp) exp]
    ; if symbol call the function that find out if it is constant or parameter of function
    [(symbol? exp) (arg->num exp local-env)]
    [(list? exp) 
     (let ([op (car exp)]
           [args (map (lambda (x) (eval-num-exp x local-env)) (cdr exp))])
       (case op
         [(+) (apply + args)]
         [(-) (apply - args)]
         [(*) (apply * args)]
         [(/) (apply / args)]
         [(floor) (floor (car args))]
         [(cos) (cos (car args))]
         [(sin) (sin (car args))]
         [else (error "Unknown numeric operation")]))]
    [else (error "Invalid numeric expression")]))

; Evaluate boolean expressions
(define (eval-bool-exp exp [local-env (make-hash)])
  (let ([op (car exp)]
        [args (map (lambda (x) (eval-num-exp x local-env)) (cdr exp))])
    (case op
      [(=) (= (first args) (second args))]
      [(<) (< (first args) (second args))]
      [(>) (> (first args) (second args))]
      [else (error "Unknown boolean operation")])))

; Evaluates an expression in the given environment

(define (evaluate-expression expr [local-env (make-hash)])
  ; (displayln expr)
  (cond
    ; Check if the expression is a number (literal value)
    [(number? expr) expr]
    ; Check if the expression is a string (for styles, etc.)
    [(string? expr) expr]
    ; Check if the expression is a symbol, which could be a variable or constant name
    [(symbol? expr) (hash-ref env expr (error "Undefined symbol: ~a" expr))]
    ; Handle arithmetic and boolean operations
    [(list? expr)
     (let ([op (car expr)])
       (case op
         ; SVG operations (circle, rect, line) are handled in eval-application
         [(circle rect line) (eval-application expr local-env)]
         ; Arithmetic operations
         [(+ - * / floor cos sin) (eval-num-exp expr local-env)]
         ; Boolean operations
         [(= < >) (eval-bool-exp expr local-env)]
         ; if expression
         [(if) (evaluate-if expr local-env)]
         ; when expression
         [(when) (evaluate-when expr local-env)]
         ; Assume it's a function call if none of the above match
         [else (eval-application expr local-env)]))]
    [else (error "Unknown expression type")]))


; Placeholder for `evaluate-if` function

(define (evaluate-if expr [local-env (make-hash)])
  (let* ([test-expr (cadr expr)]
         [true-expr (caddr expr)]
         [false-expr (cadddr expr)]
         [test-result (evaluate-expression test-expr local-env)])
    (if test-result
        (evaluate-expression true-expr local-env)
        (evaluate-expression false-expr local-env))))

; Placeholder for `evaluate-when` function
(define (evaluate-when expr local-env)
  (let ([test-expr (cadr expr)])
    (when (evaluate-expression test-expr local-env)
      (for-each (lambda (e) (evaluate-expression e local-env))
                (cddr expr)))))

; Evaluate applications (SVG operations and function calls)
(define (eval-application expr [local-env-p (make-hash)])
  ; (displayln expr)
  (let ([op (car expr)]
        ; [args (map eval-arg (cdr app))]
        [args (cdr expr)]
        )
    (if (list? op) (eval-application op local-env-p)
    ; (displayln "Super")
    ; (displayln op)
    (case op
      [(circle) (set! svg-markup (string-append svg-markup (circle (list-ref args 0) (list-ref args 1) (list-ref args 2) (list-ref args 3) local-env-p)))]
      [(rect) (set! svg-markup (string-append svg-markup (rect (list-ref args 0) (list-ref args 1) (list-ref args 2) (list-ref args 3) (list-ref args 4) local-env-p)))]
      ; [(rect) (displayln "Super")]
      [(line) (format "<line x1='~a' y1='~a' x2='~a' y2='~a' style='~a'/>" (list-ref args 0) (list-ref args 1) (list-ref args 2) (list-ref args 3))]
      [else
       (let ([fun (hash-ref env op)])
         (if fun
             (let* ([params (car fun)]
                    [body (cdr fun)]
                    [local-env (make-hash)])
               ; Populate local-env with argument bindings
               (for-each (lambda (param arg) (hash-set! local-env param (arg->num arg local-env-p)))
                         params
                         args)
               ; (displayln "parameters in application")
               ; (for ([key (hash-keys local-env)])   (printf "Key: ~a, Value: ~a\n" key (hash-ref local-env key)))
               ; Evaluate the function body in the context of local-env
               ; Placeholder for evaluating the body goes here
               (displayln fun)
               (eval-body body local-env)
               )
             (error "Function not defined: ~a" op)))]))))

; Placeholder function for evaluating the body of a function
(define (eval-body body local-env)
  ; Assuming 'body' is a list of expressions, evaluate each in order
  ; This is a simplification; you'll need to handle the actual evaluation logic
  (displayln body))
  ; (for-each (lambda (expr) (evaluate-expression expr local-env)) body))

#|
; Evaluate arguments
(define (eval-arg arg)
  (cond
    [(string? arg) arg]
    [(number? arg) arg]
    [(symbol? arg) (if (string? (hash-ref env arg false))
                       (hash-ref env arg)
                       (eval-num-exp arg))]
    [else (error "Invalid argument type")]))
|#

; Main interpreter function
(define (execute width height prg expr)
  (for-each process-definition prg)
  (eval-application expr)
  (string-append "<svg width=\"" (number->string width) "\" height=\"" (number->string height) "\">"
                 svg-markup
                 "\n</svg>"))

; Process each definition and add it to the environment
(define (process-definition line)
  (match line
    [(list 'define (cons name params) body ...) (define-var name (cons params (list body)))]
    [(list 'define name value) (define-var name value)]))


; Placeholder for executing your SVG language program
; (execute 400 400 your-program-here your-expression-here)

(define test1
  '((define (start)
      (rect 0 0 100 100 "fill:red")
      (rect 100 0 100 100 "fill:green")
      (rect 200 0 100 100 "fill:blue"))))

(define test2
  '((define STYLE "fill:red;opacity:0.2;stroke:red;stroke-width:3")
    (define START 195)
    (define END 10)
    (define (circles x r)
      (when (> r END)
        (circle x 200 r STYLE)
        (circles (+ x (floor (/ r 2))) (floor (/ r 2)))))))

(display (execute 400 400 test1 '(start)))
; (display (execute 400 400 test2 '(circles 200 START)))
; (displayln svg-markup)
; (for ([key (hash-keys env)])   (printf "Key: ~a, Value: ~a\n" key (hash-ref env key)))

#|
(arg->num 'STYLE)
(arg->num 'r)
(arg->num '(* PI 1.5))

(evaluate-expression '(* START 1.5))
(evaluate-expression '(+ (floor (- START 50)) (floor (* START 1.5))))
|#