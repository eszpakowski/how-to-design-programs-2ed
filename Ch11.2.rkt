;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ch11.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)


; List-of-numbers -> List-of-numbers 
; produces a sorted version of alon
(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-satisfied (sort> (list 12 20 -5)) sorted>?)

(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [else (insert (first alon)
                  (sort>(rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 3 (list 2 1)) (list 3 2 1))
(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))

(define (insert n alon)
  (cond
    [(empty? alon) (cons n alon)]
    [else (if (>= n (first alon))
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))

; List-of-numbers -> Boolean
(define (sorted>? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [else (if (> (first ne-l)
                 (first (rest ne-l)))
              (sorted>? (rest ne-l))
              #false)]))