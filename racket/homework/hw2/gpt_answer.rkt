#lang racket

(define (execute width height prg expr)
  (define constants (extract-constants prg))
  (define functions (extract-functions prg))
  (define environment (merge-environments constants functions))
  (define result (evaluate expr environment))
  (generate-svg width height result))

(define (extract-constants prg)
  (filter constant-definition? prg))

(define (constant-definition? definition)
  (and (list? definition)
       (= 3 (length definition))
       (eq? 'define (car definition))
       (symbol? (cadr definition))
       (or (string? (caddr definition)) (number? (caddr definition)))))

(define (extract-functions prg)
  (filter function-definition? prg))

(define (function-definition? definition)
  (and (list? definition)
       (>= (length definition) 3)
       (eq? 'define (car definition))
       (pair? (cadr definition))
       (symbol? (caadr definition))
       (map symbol? (cdadr definition))))

(define (merge-environments constants functions)
  (append constants functions))

(define (evaluate expr environment)
  (cond ((symbol? expr) (lookup-variable expr environment))
        ((number? expr) expr)
        ((string? expr) expr)
        ((list? expr) (evaluate-expression expr environment))
        (else (error "Invalid expression" expr))))

(define (lookup-variable var env)
  (let ((binding (assoc var env)))
    (if binding
        (cdr binding)
        (error "Undefined variable" var))))

(define (evaluate-expression expr environment)
  (define op (car expr))
  (case op
    ((if) (evaluate-if expr environment))
    ((when) (evaluate-when expr environment))
    (else (apply-function op (cdr expr) environment))))

(define (evaluate-if expr environment)
  (let ((condition (evaluate (cadr expr) environment)))
    (if condition
        (evaluate (caddr expr) environment)
        (evaluate (cadddr expr) environment))))

(define (evaluate-when expr environment)
  (let ((condition (evaluate (cadr expr) environment)))
    (if condition
        (apply (lambda (exp) (evaluate exp environment)) (cddr expr))
        "")))

(define (apply-function func args environment)
  (case func
    ((circle) (apply-circle args environment))
    ((rect) (apply-rect args environment))
    ((line) (apply-line args environment))
    (else (apply-user-defined-function func args environment))))

(define (apply-circle args environment)
  (let ((x (evaluate (cadr args) environment))
        (y (evaluate (caddr args) environment))
        (r (evaluate (cadddr args) environment))
        (style (evaluate (caddr (cddddr args)) environment)))
    (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" x y r style)))

(define (apply-rect args environment)
  (let ((x (evaluate (cadr args) environment))
        (y (evaluate (caddr args) environment))
        (width (evaluate (cadddr args) environment))
        (height (evaluate (caddr (cddddr args)) environment))
        (style (evaluate (cadr (cddddr args)) environment)))
    (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x y width height style)))

(define (apply-line args environment)
  (let ((x1 (evaluate (cadr args) environment))
        (y1 (evaluate (caddr args) environment))
        (x2 (evaluate (cadddr args) environment))
        (y2 (evaluate (caddr (cddddr args)) environment))
        (style (evaluate (cadr (cddddr args)) environment)))
    (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1 y1 x2 y2 style)))

(define (apply-user-defined-function func args environment)
  (let ((definition (assoc func environment)))
    (if definition
        (let ((params (cdadr definition))
              (body (cddr definition)))
          (let ((new-env (extend-environment params args environment)))
            (apply (lambda (exp) (evaluate exp new-env)) body)))
        (error "Undefined function" func))))

(define (extend-environment params args env)
  (if (null? params)
      env
      (cons (cons (car params) (car args))
            (extend-environment (cdr params) (cdr args) env))))

(define (generate-svg width height content)
  (format "<svg width=\"~a\" height=\"~a\">~a</svg>" width height content))


(define prg
  '((define STYLE "fill:pink;opacity:0.5;stroke:black;stroke-width:2")
    (define END 15)
    (define (recur-circ x y r)
       (circle x y r STYLE)
       (when (> r END)
         (recur-circ (+ x r) y (floor (/ r 2)))
         (recur-circ (- x r) y (floor (/ r 2)))
         (recur-circ x (+ y r) (floor (/ r 2)))
         (recur-circ x (- y r) (floor (/ r 2)))))))

(execute 500 500 prg '(recur-circ 200 200 100))
