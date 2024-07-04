#lang racket

; (provide minimum-spanning-tree (struct-out edge) graph)
(struct edge (u v weight) #:transparent)
(struct graph (nodes edges) #:transparent)

(define gr (graph '(A B C D E F)
                     (list (edge 'A 'B 1)
                           (edge 'D 'E 4)
                           (edge 'E 'F 7)
                           (edge 'A 'D 5)
                           (edge 'B 'E 2)
                           (edge 'C 'F 5)
                           (edge 'D 'B 6)
                           (edge 'E 'C 4)
                           (edge 'A 'E 3))))

(define (reverse-edge e)
  (match e
    [(edge u v w) (edge v u w)]))

(define (extend-graph g)
  (let* ([nodes (graph-nodes g)]
         [edges (graph-edges g)])
  (graph nodes (append edges (map reverse-edge edges)))))

(define (filter-cover-uncover edges covered uncovered)
  (filter (lambda (e)
             (and (member (edge-u e) covered)
                  (member (edge-v e) uncovered)))
          edges))

(define (find-edge g covered uncovered)
  (let* ([filter-cov-unc (filter-cover-uncover (graph-edges g) covered uncovered)]
         [sorted-cov-unc (sort filter-cov-unc (lambda (e1 e2) (< (edge-weight e1) (edge-weight e2))))])
    (car sorted-cov-unc)))

(define (process-edge g covered uncovered mst-edges)
  (let* ([edge (find-edge g covered uncovered)]
         [new-covered (cons (edge-v edge) covered)]
         [new-uncovered (remove (edge-v edge) uncovered)])
    (make-mst g new-covered new-uncovered (cons edge mst-edges))))

(define (make-mst g covered uncovered mst-edges)
  (cond [(empty? uncovered) mst-edges]
        [(process-edge g covered uncovered mst-edges)]))

(define (minimum-spanning-tree g)
  (let* ([extended-g (extend-graph g)]
         [nodes (graph-nodes extended-g)]
         [covered (car nodes)]
         [uncovered (cdr nodes)])
    (make-mst extended-g (list covered) uncovered '())))
