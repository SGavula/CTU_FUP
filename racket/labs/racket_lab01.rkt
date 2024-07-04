#lang racket

(define (my-even n)
  (cond [(< n 0) (my-even (- n))] 
        [(= n 1) #f]
        [(= n 0) #t]
        [else (my-even (- n 2))]))

(my-even 10)
(my-even 5)
(my-even -5)
(my-even -10)


(define (addition num1 num2)
  (+ num1 num2))

(displayln "\nThese are the results of the second function: ")
(addition 5 1)
(addition 60 1)
(addition 5 5)

; Passing operation + or - directly as the argument
(define (calc num1 op num2)
  (op num1 num2))

(displayln "")
(calc 4 + 4)
(calc 5 - 2)
(calc 10 - 2)
(calc 5 + 21)

#|
(define (copy-str n str)
  (cond [(= n 1) str]
        [else (string-append (copy-str (- n 1) str))]))
|#

#|
(define (copy-str n str)
  (cond [(= n 1) str]
        [else (string-append str (copy-str (- n 1) str))]))

(copy-str 3 "abc")
|#

(define (copy-str n str [acc ""])
  (cond [(= n 0) acc]
        [else (copy-str (- n 1) str (string-append acc str))]))

(copy-str 10 " abc")

#|
(define (consecutive-chars fst lst)
  (cond [(= (char->integer fst) (char->integer lst)) (string fst)]
    [(< (char->integer fst) (char->integer lst)) (string-append (string fst) (consecutive-chars (integer->char (+ (char->integer fst) 1)) lst))]
    [else (string-append (string fst) (consecutive-chars (integer->char (- (char->integer fst) 1)) lst))]
))
|#

#|
(define (move-forward lst first-idx)
  (consecutive-chars (integer->char (+ first-idx 1)) lst))

(define (move-backward lst first-idx)
  (consecutive-chars (integer->char (- first-idx 1)) lst))

(define (consecutive-chars fst lst)
  (define first-idx (char->integer fst))
  (define second-idx (char->integer lst))
  (cond [(= first-idx second-idx) (string fst)]
        [(< first-idx second-idx) (string-append (string fst) (move-forward lst first-idx))]
        [else (string-append (string fst) (move-backward lst first-idx)) ]))
|#


(define (move op fst)
  (integer->char (op fst 1)))

(define (consecutive-chars fst lst [acc ""])
  (define fst-idx (char->integer fst))
  (define lst-idx (char->integer lst))
  (define fst-str (string fst))
  
  (cond [(= fst-idx lst-idx) (string-append acc fst-str)]
        [(< fst-idx lst-idx) (consecutive-chars (move + fst-idx) lst (string-append acc fst-str))]
        [else (consecutive-chars (move - fst-idx) lst (string-append acc fst-str))]))

(consecutive-chars #\A #\D)
(consecutive-chars #\z #\u)

#|
(define (num-of-digits n)
  (if (= n 1) 1 (+ (num-of-digits (quotient n 10)) 1)))
|#

(define (num-of-digits n)
  (cond [(< n 0) (num-of-digits (- n))]
        [(< n 9) 1]
        [else (+ (num-of-digits (quotient n 10)) 1)]))
  
(num-of-digits 400044500)
(num-of-digits -123)

;(num-of-digits 450)

(define (dec->bin n [acc ""])
  (cond [(= n 1) (string-append (number->string 1) acc)]
        [(= n 0) (string-append (number->string 0) acc)]
        [else (dec->bin (quotient n 2) (string-append (number->string (modulo n 2)) acc))]))

(define (hexa-modulo n)
  (cond [(< n 10) (number->string n)]
        [else (string (integer->char (+ n 55)))]))

(define (dec->hex n [acc ""])
  (cond [(< n 16) (string-append (hexa-modulo n) acc)]
        [else (dec->hex (quotient n 16) (string-append (hexa-modulo (modulo n 16)) acc)) ]))

(define (num->str n [radix 10])
  (cond [(= radix 2) (dec->bin n)]
        [(= radix 16) (dec->hex n)]
        [else (number->string n)]))

(num->str 52)
(num->str 5 2)
(num->str 255 16)
(num->str 479 16)
(num->str 894 16)