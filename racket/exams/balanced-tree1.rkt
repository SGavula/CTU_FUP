#lang racket
; (provide node node-v node-left node-right is-leaf? build-heap)
(struct node (v left right) #:transparent)
(define (is-leaf? nd)
  (eq? 'leaf nd))

(define (show-tree tree [depth 0])
  (define (offset d)
    (if (= d 0)
        ""
        (string-append "---" (offset (- d 1)))))
  (if (is-leaf? tree)
      tree
      (begin
      (show-tree (node-left tree) (+ depth 1))
      (displayln (string-append (offset depth) (number->string (node-v tree))))
      (show-tree (node-right tree) (+ depth 1))
       tree)
      ))

(define (min-depth tree)
  (if (is-leaf? tree) 0 (+ 1 (min (min-depth (node-left tree)) (min-depth (node-right tree))))))

(define (heapify-left-tree tree)
  (let* ([value (node-v tree)]
         [left-tree (node-left tree)]
         [left-value (node-v left-tree)]
         [new-value (max value left-value)]
         [new-left-value (min value left-value)])
    (node new-value (node new-left-value 'leaf 'leaf) 'leaf)))

(define (heapify tree)
  (let* ([value (node-v tree)]
         [left-tree (node-left tree)]
         [left-value (node-v left-tree)]
         [llt (node-left left-tree)]
         [lrt (node-right left-tree)]
         [right-tree (node-right tree)]
         [right-value (node-v right-tree)]
         [rlt (node-left right-tree)]
         [rrt (node-right right-tree)]
         [balanced-left-tree (if (> left-value value) (node left-value (node value llt lrt) right-tree) (node value left-tree right-tree))]
         [new-value (node-v balanced-left-tree)]
         [new-left-tree (node-left balanced-left-tree)]
         [new-right-tree (node-right balanced-left-tree)])
    (if (> right-value new-value) (node right-value new-left-tree (node new-value rlt rrt)) (node new-value new-left-tree new-right-tree))))

(define (enforce-heap tree)
  (cond [{is-leaf? tree} 'leaf]
        [(is-leaf? (node-right tree)) (heapify-left-tree tree)]
        [(heapify tree)]))

(define (build-tree value tree)
  (cond [(is-leaf? tree) (node value 'leaf 'leaf)]
        [(< (min-depth (node-right tree)) (min-depth (node-left tree))) (enforce-heap (node (node-v tree) (node-left tree) (build-tree value (node-right tree))))]
        [(enforce-heap (node (node-v tree) (build-tree value (node-left tree)) (node-right tree)))]))

(define (build-heap nodes)
  (foldr build-tree 'leaf nodes))