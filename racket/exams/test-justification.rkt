#lang racket

(define words '("This" "is" "an" "example" "of" "text" "justification."))
(define words1 '("What" "must" "be" "acknowledgment" "shall" "be"))
(define max-width 16)

(define words-res '("This    is    an" "example  of text" "justification.  "))
(define words1-res '("What   must   be" "acknowledgment  " "shall be        "))

(define (add-reminder line reminder)
  (cond [(equal? reminder 0) line]
        [(cons (string-append (car line) " ") (add-reminder (cdr line) (- reminder 1)))]))

(define (helper word max-width line len output)
  (cond [(equal? word "") (reverse output)]
        [(add-line "" '() max-width (list word) (string-length word) output)]))

(define (add-line-last max-width line len output)
  (let* ([line-with-spaces (map (lambda (x) (string-append x " ")) line)]
         [curr-width (+ len (length line))]
         [spaces (- max-width curr-width)]
         [spaces-string (list->string (make-list spaces '#\space))]
         [final-line (append line-with-spaces (list spaces-string))]
         [new-output (cons final-line output)])
    (reverse new-output)))

(define (check-end word max-width line len output)
  (let* ([line-len (- (length line) 1)]
         [word-len (string-length word)]
         [new-len (+ word-len len)]
         [width (+ new-len line-len)])
           (if (> width max-width)
               (add-line word '() max-width (reverse line) len output)
               (add-line-last max-width (reverse (cons word line)) (+ len (string-length word)) output))))

(define (add-line word words-lst max-width line len output)
  (let* ([num-words (max 1 (- (length line) 1))]
         [avail-space (- max-width len)]
         [spaces (floor (/ avail-space num-words))]
         [spaces-string (list->string (make-list spaces '#\space))]
         [reminder (modulo avail-space num-words)]
         [line-with-spaces (map (lambda (x) (string-append x spaces-string)) (take line num-words))]
         [line-with-reminder (add-reminder line-with-spaces reminder)]
         [line-with-last-word (if (equal? (length line) 1) line-with-reminder (append line-with-reminder (list (last line))))]
         [new-output (cons line-with-last-word output)])
    (if (empty? words-lst) (helper word max-width line len new-output) (iter word words-lst max-width '() 0 new-output))))

(define (iter word words-lst max-width line len output)
  (cond [(empty? words-lst) (check-end word max-width line len output)]
        [
         (let* ([line-len (- (length line) 1)]
                [word-len (string-length word)]
                [new-len (+ word-len len)]
                [width (+ new-len line-len)])
           (if (> width max-width)
               (add-line word words-lst max-width (reverse line) len output)
               (iter (car words-lst) (cdr words-lst) max-width (cons word line) new-len output)))]))

(define (justify words-lst max-width)
  (let* ([processed-lst (iter (car words-lst) (cdr words-lst) max-width '() 0 '())])
    (map (lambda (x) (apply string-append x) ) processed-lst)))