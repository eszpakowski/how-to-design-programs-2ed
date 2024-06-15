;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch5.7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

; Movie is a structure:
; (make-movie String String Number)
; interpretation: movie with title,
; producer name and surname, and year of production
(define-struct movie [title producer year])

; Person is a structure:
; (make-person String String String String)
; interpretation: person's name, hair and eye color,
; and phone number
(define-struct person [name hair eyes phone])

; Pet is a structure:
; (make-pet String Nubber)
; interpretation: pet's name and ID number
(define-struct pet [name number])

; CD is a structure:
; (make-CD String String Number)
; interpretation: music CD's artist name,
; CD's title and its price
(define-struct CD [artist title price])

; A Material is one of:
; - polyester
; - wool
; - etc

; Swaeter is a structure:
; (make-sweater Material Number String)
; interpretation: sweater's material,
; its size and producer name
(define-struct sweater [material size producer])


; Time is a structure:
; (make-time Number Number Number)
; interpretation: a point in time consisting of three numbers:
; hours, minutes, and seconds
(define-struct time [hours minutes seconds])

; Time -> Number
; produces the number of seconds that have passed since midnight
(check-expect (second-since-midnight (make-time 12 30 2)) 45002)
(define (second-since-midnight t)
  (+
   (* 60 60 (time-hours t))
   (* 60 (time-minutes t))
   (time-seconds t)))

; MatchingLetter is one of:
; - 1String: lowercase letter a through z
; - #false

; ThreeLetter is a structure:
; (make-three-letter MatchingLetter MatchingLetter MatchingLetter)
; interpretation: representing three letter word with matching information
(define-struct three-letter [a b c])

; ThreeLetter ThreeLetter -> ThreeLetter
; produces a word that indicates where the given ones agree and disagree
; retains the content if the two agree; otherwise it places #false
(check-expect (compare-word
               (make-three-letter "x" "y" "z")
               (make-three-letter "x" "y" "z"))
              (make-three-letter "x" "y" "z"))
(check-expect (compare-word
               (make-three-letter "a" "b" "c")
               (make-three-letter "x" "y" "z"))
              (make-three-letter #false #false #false))
(check-expect (compare-word
               (make-three-letter "x" "b" "z")
               (make-three-letter "x" "y" "z"))
              (make-three-letter "x" #false "z"))

(define (compare-word t1 t2)
  (make-three-letter
   (compare-letter (three-letter-a t1) (three-letter-a t2))
   (compare-letter (three-letter-b t1) (three-letter-b t2))
   (compare-letter (three-letter-c t1) (three-letter-c t2))))

; MatchingLetter MatchingLetter -> MatchingLetter
; compares two letters
; returns the same letter if they're the same
; returns #false otherwise
(check-expect (compare-letter "a" "a") "a")
(check-expect (compare-letter "b" "a") #false)
(check-expect (compare-letter "a" "b") #false)

(define (compare-letter l1 l2)
  (if (string=? l1 l2) l1 #false))
