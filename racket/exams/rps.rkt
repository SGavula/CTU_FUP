#lang racket
(provide rps)

(define players '("alice" "bob" "charlie"))
(define strategies '((r p) (r r) (s p)))

(define players1 '("alice" "bob" "charlie"))
(define strategies1 '((r p r) (p s r) (s r p)))

(define (game-finished? strats) (or (null? strats) (ormap null? strats)))
(define (strats-current strats) (map car strats))
(define (strats-future strats) (map cdr strats))

(define (match-sym-to-remove strats)
  (match strats
    ['(p r) 'r]
    ['(p s) 'p]
    ['(r s) 's]
    [_ 'x]))

(define (make-mask strats)
  (let* ([sorted-strats (sort (remove-duplicates strats) symbol<?)]
         [sym-to-remove (match-sym-to-remove sorted-strats)])
    (map (lambda (x) (if (equal? x sym-to-remove) #f #t)) strats)))

(define (mask-filter lst mask)
  (for/list ([x lst]
             [m mask] #:when m) x))

(define (rps players strategies)
  (if (game-finished? strategies)
    players
    (let* ([current (strats-current strategies)]
           [future (strats-future strategies)]
           [mask (make-mask current)])
      (rps (mask-filter players mask) (mask-filter future mask)))))

; (rps (mask-filter players mask) (mask-filter future mask))