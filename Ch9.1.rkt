;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch9.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; List-of-Amounts is one of:
; - '()
; - (cons PositiveNumber List-of-Amounts)
(define loa1 (cons 10 '()))
(define loa2 (cons 20
                   (cons 10 '())))
(define loa3 (cons 30
                   (cons 20
                         (cons 10 '()))))

; List-of-Numbers is one of:
; - '()
; - (cons Number List-of-Numbers)

; List-of-Numbers -> Boolean
; determines whether all numbers are positive numbers
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 20
                          (cons 10 '())))
              #true)
(check-expect (pos? (cons 20
                          (cons -10 '())))
              #false)
(check-expect (pos? (cons 30
                          (cons 20
                                (cons -10 '()))))
              #false)
(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [else (and (positive? (first lon))
               (pos? (rest lon)))]))


; List-of-Numbers -> PositiveNumber
; computes the sum of amounts if the argument is
; a list of positive numbers
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 20
                                 (cons 10 '())))
              30)
(check-error (checked-sum (cons 20
                                (cons -10 '())))
             "Not a positive number!")
(check-error (checked-sum (cons 30
                                (cons 20
                                      (cons -10 '()))))
             "Not a positive number!")
(check-expect (checked-sum (cons 30
                                 (cons 20
                                       (cons 10 '()))))
              60)

(define (checked-sum loa)
  (cond [(pos? loa) (sum loa)]
        [else (error "Not a positive number!")]))

; List-of-Amounts -> PositiveNumber
; computes the sum of amounts
(check-expect (sum loa1) 10)
(check-expect (sum loa2) 30)
(check-expect (sum loa3) 60)

(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else (+ (first loa)
             (sum (rest loa)))]))