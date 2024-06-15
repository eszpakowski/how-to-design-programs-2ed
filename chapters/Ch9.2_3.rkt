;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch9.2_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 

; NEList-of-temperatures -> Boolean
; produces #true if the temperatures are sorted in descending order
; that is, if the second is smaller than the first, the third smaller than the second, and so on
; otherwise produces #false
(check-expect (sorted>? (cons 24 '())) #true)
(check-expect (sorted>? (cons 25
                              (cons 24 '())))
              #true)
(check-expect (sorted>? (cons 24
                              (cons 25 '())))
              #false)
(check-expect (sorted>? (cons 26
                              (cons 25
                                    (cons 24 '()))))
              #true)
(check-expect (sorted>? (cons 25
                              (cons 26
                                    (cons 24 '()))))
              #false)

(define (sorted>? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [else (if (> (first ne-l)
                 (first (rest ne-l)))
              (sorted>? (rest ne-l))
              #false)]))


; NEList-of-temperatures -> Number
; computes the average temperature 
(check-expect (average (cons 1 (cons 2 (cons 3 '()))))
              2)
 
(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))


; NEList-of-temperatures -> Number
; computes the sum of the given temperatures 
(check-expect
 (sum (cons 1 (cons 2 (cons 3 '())))) 6)

(define (sum ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum (rest ne-l)))]))


; NEList-of-temperatures -> Number
; counts elemts in list
(check-expect (how-many (cons 24 '())) 1)
(check-expect (how-many (cons 25
                              (cons 24 '()))) 2)
(check-expect (how-many (cons 26
                              (cons 25
                                    (cons 24 '())))) 3)

(define (how-many ne-l)
  (cond
    [(empty? (rest ne-l)) 1]
    [else (+ 1 (how-many (rest ne-l)))]))


; NEList-of-Booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-Booleans)

; NEList-of-Booleans -> Boolean
; determines whether all values are true
(check-expect (all-true (cons #true '()))
              #true)
(check-expect (all-true (cons #false '()))
              #false)
(check-expect (all-true (cons #true
                              (cons #true '())))
              #true)
(check-expect (all-true (cons #true
                              (cons #true
                                    (cons #true '()))))
              #true)
(check-expect (all-true (cons #true
                              (cons #false
                                    (cons #true '()))))
              #false)

(define (all-true l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (and (first l)
               (all-true (rest l)))]))

; NEList-of-Booleans -> Boolean
; determines whether at least one item is true
(check-expect (one-true (cons #true '()))
              #true)
(check-expect (one-true (cons #false '()))
              #false)
(check-expect (one-true (cons #false
                              (cons #false '())))
              #false)
(check-expect (one-true (cons #true
                              (cons #true
                                    (cons #true '()))))
              #true)
(check-expect (one-true (cons #false
                              (cons #false
                                    (cons #true '()))))
              #true)

(define (one-true l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (or (first l)
              (one-true (rest l)))]))
