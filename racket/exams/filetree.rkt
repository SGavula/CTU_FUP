#lang racket

(define files (list "src/tree.hs"
                    "src/complex.hs"
                    "scripts/ex1/test.ss"
                    "scripts/ex1/eval.ss"
                    "scripts/emptydir"
                    "scripts/ex2/test.ss"
                    "tests/test_tree.hs"))

(define (split file) (string-split file "/"))

; file '("scripts" "ex1" "eval.ss")
(define (insert path tree)
  (if (empty? path) tree
      (let* ([name (car path)]
             [loc-tree (if (dict-has-key? tree name) (dict-ref tree name) #hash())]
             [updated-tree (insert (cdr path) loc-tree)])
        (dict-set tree name updated-tree))))

(define (parse files)
  (foldl (lambda (path tree) (insert (split path) tree)) #hash() files))

(define (check-in-tree name tree)
  (dict-has-key? tree name))

(define (check file-lst tree)
  (if (empty? file-lst) #\t
      (let* ([in-tree (check-in-tree (car file-lst) tree)])
        (if in-tree (check (cdr file-lst) (dict-ref tree (car file-lst)) ) #f))))

(define (exists file tree)
  (check (string-split file "/") tree))