;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch9.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N -> Number
; computes (+ n pi) without using +
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; N Number -> Number
; computes (+ n x) without using +
(check-expect (add 3 105) 108)
(check-expect (add 100 11) 111)
(check-expect (add 100 0) 100)
(check-expect (add 100 -10) 90)
 
(define (add n x)
  (cond
    [(zero? n) x]
    [else (add1 (add (sub1 n) x))]))

; N Number -> Number
; computes (* n x) without using *
(check-expect (multiply 3 4) 12)
(check-expect (multiply 5 5) 25)
(check-expect (multiply 11 99) 1089)

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [else (add x (multiply (sub1 n) x))]))
