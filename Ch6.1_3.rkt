;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch6.1_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Elephant is a Number
; interpretation represents space in square meters
; needed to transport a specific elephant

(define-struct spider [n s])
; Spider is a structure:
;    (make-spider Number Number).
; interpretation spider with n legs
; which takes up space s when needed
; to be transported
(make-spider 8 10)

(define-struct boa-con [len girth])
; BoaCon is a structure:
;    (make-boa-con Number Number).
; interpretation boa constrictor with specific length and girth

(define-struct armadillo [w h s])
; Armadillo is a structure:
;    (make-armadillo Number Number Number).
; interpretation armadillo with width w, height h and
; space in square meters s needed to transport a specific armadillo

; Cage is a Number
; interpretation cage in a specific size in square meters

; Animal is one of:
; - Elephant
; - (make-spider Number Number)
; - (make-boa-con Number Number)
; - (make-armadillo Number Number Number)
; interpretation represents any of the supported
; animal types

; Animal Cage -> Boolean
; determines whether the cage c volume is large enough
; for the specified animal a
(check-expect (fits? 7 10) #true)
(check-expect (fits? 10 10) #true)
(check-expect (fits? 11 10) #false)
(check-expect (fits? (make-spider 8 7) 10) #true)
(check-expect (fits? (make-spider 8 10) 10) #true)
(check-expect (fits? (make-spider 8 11) 10) #false)
(check-expect (fits? (make-boa-con 2 2) 6) #true)
(check-expect (fits? (make-boa-con 2 3) 6) #true)
(check-expect (fits? (make-boa-con 2 4) 6) #false)
(check-expect (fits? (make-armadillo 2 2 4) 6) #true)
(check-expect (fits? (make-armadillo 2 3 6) 6) #true)
(check-expect (fits? (make-armadillo 2 4 8) 6) #false)

(define (fits? a c)
  (cond
    [(number? a)
     (>= c a)]
    [(spider? a)
     (>= c (spider-s a))]
    [(boa-con? a)
     (>= c (* (boa-con-len a)(boa-con-girth a)))]
    [(armadillo? a)
     (>= c (armadillo-s a))]))
