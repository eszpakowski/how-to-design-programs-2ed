;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ch12_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))


; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
(define assoc1 (list "first" #true))
(define assoc2 (list "Play Count" 2))
(define assoc3 (list "Total Time" 284865))
(define assoc4 (list "fourth" (create-date 2010 5 31 18 52 27)))

; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
(define lassoc1 (list assoc1 assoc2 assoc3 assoc4))
 
; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
(define llist1 (list lassoc1))

; String LAssoc Any -> Association
; produces first assoc whose first item is equal to key
; or default if theres no matching assoc
(check-expect (find-association "incorrect" lassoc1 "not found")
              "not found")
(check-expect (find-association "first" lassoc1 "not found")
              assoc1)
(check-expect (find-association "Play Count" lassoc1 "not found")
              assoc2)
(check-expect (find-association "Total Time" lassoc1 "not found")
              assoc3)
(check-expect (find-association "fourth" lassoc1 "not found")
              assoc4)

(define (find-association key la default)
  (cond
    [(empty? la) default]
    [else
     (if (string=? (first (first la)) key)
         (first la)
         (find-association key (rest la) default))]))

(define (find-association2 key la default)
  (if (false? (assoc key la))
      default
      (assoc key la)))

; LLists -> Number
; produces total amount of play time
(check-expect (total-time/list llist1)
              569730)
              
(define (total-time/list l)
  (cond
    [(empty? l) 0]
    [else (+ (sum-total-time (first l))
             (total-time/list (rest l)))]))

; Association -> Number
(define (sum-total-time a)
  (* (get-value "Play Count" a)
     (get-value "Total Time" a)))

; String Association -> Number
(define (get-value s a)
  (if (false? (assoc s a))
      0
      (second (assoc s a))))

; LLists -> List-of-strings
; produces strings that are assocs with boolean attribute
(check-expect (boolean-attributes llist1)
              (list "first"))

(define (boolean-attributes l)
  (create-set (filter-booleans (flatten l))))

; List-of-strings -> List-of-strings
(define (create-set l)
  (cond
    [(empty? l) '()]
    [else
     (add-if-not-present (first l)
                         (create-set (rest l)))]))

; String List-of-strings -> List-of-strings
(define (add-if-not-present s l)
  (cond
    [(empty? l) (cons s l)]
    [else
     (if (member? s l)
         l
         (cons s l))]))

; LAssoc -> List-of-strings
(define (filter-booleans l)
  (cond
    [(empty? l) '()]
    [else (add-if-bool (first l)
                       (filter-booleans (rest l)))]))

; Association -> List-of-strings
(define (add-if-bool a l)
  (if (boolean? (second a))
      (cons (first a) l)
      l))

; LLists -> LAssoc
(define (flatten l)
  (cond
    [(empty? l) '()]
    [else (append (first l)
                  (flatten (rest l)))]))

; LAssoc -> Track
; converts list of assocs to a track struct
(check-expect (track-as-struct (list (list "Name" "Smells Like Teen Spirit")
                                 (list "Artist" "Nirvana")
                                 (list "Album" "Nevermind")
                                 (list "Total Time" 301165)
                                 (list "Track Number" 1)
                                 (list "Date Added" (create-date 2011 2 27 21 45 46))
                                 (list "Play Count" 2)
                                 (list "Play Date" (create-date 2011 2 27 21 58 34))))
              (create-track "Smells Like Teen Spirit" "Nirvana" "Nevermind" 301165 1 (create-date 2011 2 27 21 45 46) 2 (create-date 2011 2 27 21 58 34)))

(define (track-as-struct a)
  (create-track
   (second (assoc "Name" a))
   (second (assoc "Artist" a))
   (second (assoc "Album" a))
   (second (assoc "Total Time" a))
   (second (assoc "Track Number" a))
   (second (assoc "Date Added" a))
   (second (assoc "Play Count" a))
   (second (assoc "Play Date" a))))