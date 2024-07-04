#lang racket
(provide find-path
         common-ancestor
         (struct-out node)
         (struct-out leaf))


(struct node (val left right) #:transparent)
(struct leaf (val) #:transparent)

(define tree (node 1 (node 2 (leaf 5) (leaf 6))
                     (node 3 (leaf 4) (leaf 7))))

(define tree2 (node 1 (node 2 (leaf 3)
                              (node 4 (leaf 5)
                                      (leaf 6)))
                      (node 7 (leaf 8)
                              (leaf 9))))

(define (find-path x tree)
  (match tree
    [(leaf v) (if (equal? v x) (list x) '())]
    [(node v left right) (if (equal? v x)
                             (list x)
                             (let* ([l (find-path x left)]
                                    [r (find-path x right)]
                                    [both (append l r)])
                               (if (empty? both) '() (cons v both))))]))

(define (common-ancestor x y tree)
  (let* ([x-path (find-path x tree)]
         [y-path (find-path y tree)]
         [ancestor (take-common-prefix x-path y-path)])
    (if (empty? ancestor) #f (last ancestor))))


#|
(define (pattern-match tree)
  (match tree
    [(node v left right) (display right)]))
|#