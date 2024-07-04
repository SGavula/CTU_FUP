#lang racket

;list-ref vracia prvok y pola, niečo ako v proceduralnom jazyku arr[0]
(define my-list '(a b c d e))
(list-ref my-list 1) ; returns 'b
(list-ref my-list 0) ; returns 'a

; Ak mam 2d pole
(define my-list2 '((a b) (c d) (e f)))
(list-ref my-list2 1) ; returns '(c d)
(list-ref (list-ref my-list2 1) 1) ; returns 'd

;for*/list vytvori vsetkz kombinacie
(for*/list ([i '(1 2)]
              [j '(2 4)])
    (+ i j))
; nam vrati '(3 5 4 6)

(for*/list ([i '(1 2)]
              [j "ab"])
    (list i j))
; '((1 #\a) (1 #\b) (2 #\a) (2 #\b))

; potom existuje for/list, ktorý nerobí všetky kombinácie, ale zoberie prvý prvok z prvého poľa a spojí ho s prvým prvkom z druhého poľa 
(for*/list ([i '(1 2)]
              [j '(2 4)])
    (+ i j))
; vráti mi to '(3 6), ono to vracia pole

(range 10) ; '(0 1 2 3 4 5 6 7 8 9)

(range 10 20) ; '(10 11 12 13 14 15 16 17 18 19)

