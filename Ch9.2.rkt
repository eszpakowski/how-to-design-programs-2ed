;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch9.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; List-of-Booleans is one of:
; - '()
; - (cons Boolean List-of-Booleans)
(define lob1 (cons #true '()))
(define lob2 (cons #false
                   (cons #true '())))
(define lob3 (cons #true
                   (cons #true
                         (cons #true '()))))


; List-of-Booleans -> Boolean
; determines whether all values are true
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '()))
              #true)
(check-expect (all-true (cons #false
                              (cons #true '())))
              #false)
(check-expect (all-true (cons #true
                              (cons #true
                                    (cons #true '()))))
              #true)

(define (all-true lob)
  (cond
    [(empty? lob) #true]
    [else (and (first lob)
               (all-true (rest lob)))]))

; List-of-Booleans -> Boolean
; determines whether at least one item is true
(check-expect (one-true '()) #false)
(check-expect (one-true (cons #true '()))
              #true)
(check-expect (one-true (cons #false '()))
              #false)
(check-expect (one-true (cons #false
                              (cons #true '())))
              #true)
(check-expect (one-true (cons #false
                              (cons #false
                                    (cons #true '()))))
              #true)

(define (one-true lob)
  (cond
    [(empty? lob) #false]
    [else (or (first lob)
              (one-true (rest lob)))]))


; List-of-string -> String
; concatenates all strings in l into one long string
 
(check-expect (cat '()) "")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect
  (cat (cons "ab" (cons "cd" (cons "ef" '()))))
  "abcdef")
 
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l)
                         (cat (rest l)))]))

(cat (cons "a" '()))