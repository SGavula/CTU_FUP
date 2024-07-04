#lang racket

; Environment
(define global-env (make-hash))
(define svg-markup "")

; Functions generating svg
(define (circle x y r style local-env)
  (define x-val (arg->num x local-env))
  (define y-val (arg->num y local-env))
  (define r-val (arg->num r local-env))
  (define style-val (arg->num style local-env))
  (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" x-val y-val r-val style-val))

(define (rect x y width height style local-env)
  (define x-val (arg->num x local-env))
  (define y-val (arg->num y local-env))
  (define width-val (arg->num width local-env))
  (define height-val (arg->num height local-env))
  (define style-val (arg->num style local-env))
  (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x-val y-val width-val height-val style-val))

(define (line x1 y1 x2 y2 style local-env)
  (define x1-val (arg->num x1 local-env))
  (define y1-val (arg->num y1 local-env))
  (define x2-val (arg->num x2 local-env))
  (define y2-val (arg->num y2 local-env))
  (define style-val (arg->num style local-env))
  (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1-val y1-val x2-val y2-val style-val))

; Helper functions
(define (add-to-global-env name value)
  (hash-set! global-env name value))

(define (print-global-env)
  (map (lambda (key) (printf "Key: ~a | Value: ~a\n" key (hash-ref global-env key))) (hash-keys global-env)))

(define (sym->num sym local-env)
  (define str-sym (symbol->string sym))
  (cond
    [(string=? (string-upcase str-sym) str-sym) (hash-ref global-env sym)]
    [(string=? (string-downcase str-sym) str-sym) (hash-ref local-env sym)]))

(define (arg->num arg local-env)
  (cond
    [(list? arg) (eval-num-expr arg local-env)]
    [(number? arg) arg]
    [(string? arg) arg]
    [(symbol? arg) (sym->num arg local-env)]
    [else (error "Unknown argument")]))

(define (eval-bool-expr expr local-env)
  (let ([op (car expr)]
        [args (map (lambda (x) (eval-num-expr x local-env)) (cdr expr))])
    (case op
      [(=) (= (first args) (second args))]
      [(<) (< (first args) (second args))]
      [(>) (> (first args) (second args))]
      [else (error "Unknown boolean operation")])))

(define (eval-num-expr expr local-env)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (arg->num expr local-env)]
    [(list? expr)
     (let ([op (car expr)]
           [args (map (lambda (x) (eval-num-expr x local-env)) (cdr expr))])
       (case op
         [(+) (apply + args)]
         [(-) (apply - args)]
         [(*) (apply * args)]
         [(/) (apply / args)]
         [(floor) (floor (car args))]
         [(cos) (cos (car args))]
         [(sin) (sin (car args))]
         [else (error "Unknown numeric operation")]))]))

(define (eval-when-expr expr local-env)
  (cond [(eval-bool-expr (car expr) local-env) (map (lambda (x) (eval-expression x local-env)) (cdr expr))]))

(define (eval-if-expr expr local-env)
  (let ([bool-expr (car expr)]
        [positive-expr (cadr expr)]
        [negative-expr (caddr expr)])
    (if (eval-bool-expr bool-expr local-env)
        (eval-expression positive-expr local-env)
        (eval-expression negative-expr local-env))))

(define (eval-expression expr local-env)
  (cond
    [(number? expr) expr]
    [(string? expr) expr]
    [(symbol? expr) (hash-ref global-env expr)]
    [(list? expr)
     (let ([op (car expr)])
       (case op
         [(circle rect line) (eval-app expr local-env)]
         [(+ - * / floor cos sin) (eval-num-expr expr local-env)]
         [(= < >) (eval-bool-expr expr local-env)]
         [(if) (eval-if-expr (cdr expr) local-env)]
         [(when) (eval-when-expr (cdr expr) local-env)]
         [else (eval-app expr local-env)]))]))

(define (eval-body body local-env)
  (map (lambda (x) (eval-expression x local-env)) body))

(define (eval-fun op args local-env-p)
  (define fun (hash-ref global-env op))
  (let ([params (car fun)]
        [body (if (= (length fun) 2) (cadr fun) (cdr fun))]
        [local-env (make-hash)])
    ; Add parameters of function with their values to the local environment
    (map (lambda (param arg) (hash-set! local-env param (arg->num arg local-env-p))) params args)
    (eval-body body local-env)))

(define (eval-app expr [local-env (make-hash)])
  (let ([op (car expr)]
        [args (cdr expr)])
    (if (list? op)
        (eval-app op local-env)
        (match op
          ['circle (set! svg-markup (string-append svg-markup (circle (list-ref args 0) (list-ref args 1) (list-ref args 2) (list-ref args 3) local-env)))]
          ['rect (set! svg-markup (string-append svg-markup (rect (list-ref args 0) (list-ref args 1) (list-ref args 2) (list-ref args 3) (list-ref args 4) local-env)))]
          ['line (set! svg-markup (string-append svg-markup (line (list-ref args 0) (list-ref args 1) (list-ref args 2) (list-ref args 3) (list-ref args 4) local-env)))]
          [_ (eval-fun op args local-env)]))))

(define (process-program line)
  (match line
    [(list 'define (cons name params) body ...) (add-to-global-env name (cons params (list body)))]
    [(list 'define name value) (add-to-global-env name value)]))

(define (execute width height prg expr)
  (set! svg-markup "")
  (map (lambda (x) (process-program x)) prg)
  (eval-app expr)
  (string-append "<svg width=\"" (number->string width) "\" height=\"" (number->string height) "\">"
                svg-markup
                 "</svg>"))

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

 (define tree-prg
    '((define STYLE1 "stroke:black;stroke-width:2;opacity:0.9")
      (define STYLE2 "stroke:green;stroke-width:3;opacity:0.9")
      (define FACTOR 0.7)
      (define PI 3.14)
      (define (draw x1 y1 x2 y2 len angle)
        (if (> len 30)
            (line x1 y1 x2 y2 STYLE1)
            (line x1 y1 x2 y2 STYLE2))
        (when (> len 20)
          (recur-tree x2 y2 (floor (* len FACTOR)) angle)
          (recur-tree x2 y2 (floor (* len FACTOR)) (+ angle 0.3))
          (recur-tree x2 y2 (floor (* len FACTOR)) (- angle 0.6))))
      (define (recur-tree x1 y1 len angle)
        (draw x1
              y1
              (+ x1 (* len (cos angle)))
              (+ y1 (* len (sin angle)))
              len
              angle))))

(provide execute)

; (display (execute 400 400 '() '(line 10 20 30 40 "stroke:black;stroke-width:5")))
; (display (execute 400 400 '((define STYLE "fill:red")) '(circle 200 200 (floor (/ 200 3)) STYLE)))
; (display (execute 400 400 test1 '(start)))
; (display (execute 400 400 test2 '(circles 200 START)))
; (display (execute 400 300 tree-prg '(recur-tree 200 300 100 (* PI 1.5))))