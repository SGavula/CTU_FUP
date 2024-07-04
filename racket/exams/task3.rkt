#lang racket
(provide accepts
         lwords)

(struct transition (from-state symbol to-state))
(struct automaton (trans init-state final-states))

(define nfa
  (automaton
   (list (transition 1 #\a 2)
         (transition 2 #\b 2)
         (transition 1 #\a 3)
         (transition 3 #\b 4)
         (transition 4 #\a 3)
         (transition 2 #\a 4))
   1
   (list 2 3)))

; zo zadaného stavu a symbolu mi vráti list stavov kam sa môžem dostať
(define (next-state-from-one-state state symbol transitions)
  (let* ([filtered-trans (filter (lambda (x) (and (equal? (transition-from-state x) state) (equal? (transition-symbol x) symbol))) transitions)])
    (map transition-to-state filtered-trans)))

(define (next-state-from-list-states states symbol transitions)
  (let* ([next-states (map (lambda (x) (next-state-from-one-state x symbol transitions)) states)])
    (apply append next-states)))

(define (check-last-states automaton last-states)
  (let* ([final-states (automaton-final-states automaton)]
         [filtered-last-states (map (lambda (x)(filter (lambda (y) (equal? x y)) final-states)) last-states)]
         [res-last-states (apply append filtered-last-states)]
         [len (length res-last-states)])
    (if (> len 0) #\t #\f)))

(define (accepts automaton word)
  (let* ([list-characters (string->list word)]
         [transitions (automaton-trans automaton)]
         [init-state (list (automaton-init-state automaton))]
         [last-states (foldl (lambda (x state) (next-state-from-list-states state x transitions)) init-state list-characters)])
    (check-last-states automaton last-states)))

(define (make-words str n)
  (cond [(equal? n 0) '()]
        [(equal? n 1) (map list str)]
        [(apply append (map (lambda (x) (map (lambda (y) (cons y x)) str)) (make-words str (- n 1))))]))

(define (lwords alphabet automaton n)
  (let* ([generated-characters (make-words (string->list alphabet) n)]
         [concat-to-words (map list->string generated-characters)])
    (filter (lambda (x) (equal? (accepts automaton x) #\t)) concat-to-words)))