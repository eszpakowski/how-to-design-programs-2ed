;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ch12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)


(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter Dictionary -> Number
; counts how many words in the given Dictionary start with the given Letter
(check-expect (starts-with# "f" '()) 0)
(check-expect (starts-with# "f" (list "first" "second" "third" "fourth")) 2)
(check-expect (starts-with# "s" (list "first" "second" "third" "fourth")) 1)
(check-expect (starts-with# "x" (list "first" "second" "third" "fourth")) 0)

(define (starts-with# l dic)
  (cond
    [(empty? dic) 0]
    [else (if (same-first-letter? l (first dic))
              (+ 1 (starts-with# l (rest dic)))
              (starts-with# l (rest dic)))]))

; String String -> Boolean
(define (same-first-letter? s1 s2)
  (string=? (string-ith s1 0) (string-ith s2 0)))
;;;;;;;;;;;;;;;;;;;;;

(define-struct lc [s count])
; Letter-Count is a structure:
;     (make-lc 1String Number)
; interpretation: number of words starting with the given letter

; List-of-Lc is one of:
; - '()
; - (cons Letter-Count List-of-Lc)

; Dictionary -> List-of-Lc
; counts how often each letter is used as
; the first one of a word in the given dictionary
(check-expect (count-by-letter '())'())
(check-expect (count-by-letter (list "first" "second"))
              (list (make-lc "f" 1) (make-lc "s" 1)))
(check-expect (count-by-letter (list "first" "second" "third" "fourth"))
              (list (make-lc "s" 1) (make-lc "t" 1) (make-lc "f" 2)))

(define (count-by-letter dic)
  (letter-counts (explode (first-letters dic)) dic))

; List-of-1Strings Dictionary -> List-of-Lc
(define (letter-counts letters dic)
  (cond
    [(empty? letters) '()]
    [else (cons (create-lc (first letters) dic)
                (letter-counts (rest letters) dic))]))

; 1String Dictionary -> LetterCount
(define (create-lc s dic)
  (make-lc s (starts-with# s dic)))

; Dictionary -> String
(check-expect (first-letters '()) "")
(check-expect (first-letters (list "first" "second" "third" "fourth")) "stf")
(check-expect (first-letters (list "fifth" "sixth" "seventh" "eighth")) "fse")

(define (first-letters dic)
  (cond
    [(empty? dic) ""]
    [else (append-letter-if-not-present (first dic)
                                        (first-letters (rest dic)))]))

; String String -> String
(define (append-letter-if-not-present w s)
  (if (string-contains? (string-ith w 0) s)
      s
      (string-append (string-ith w 0) s)))

;;;;;;;;;;;;;;;;;;;;;;

; Dictionary -> Letter-Count
(check-expect (most-frequent (list "first" "second" "third" "fourth"))
              (make-lc "f" 2))
(define (most-frequent dic)
  (pick-most-freq (count-by-letter dic)))

; Dictionary -> Letter-Count
(check-expect (most-frequent2 (list "first" "second" "third" "fourth"))
              (make-lc "f" 2))
(define (most-frequent2 dic)
  (first (sort (count-by-letter dic))))

; List-of-Lc -> Letter-Count
(define (pick-most-freq lcs)
  (cond
    [(empty? lcs) (make-lc "no letters found" 0)]
    [else (more-freq (first lcs)
                     (pick-most-freq (rest lcs)))]))

; LetterCount LetterCount -> LetterCount
(define (more-freq lc1 lc2)
  (if (> (lc-count lc1) (lc-count lc2))
      lc1
      lc2))

; List-of-Lc -> List-of-Lc
(check-expect (sort '()) '())
(check-expect (sort (list (make-lc "t" 3) (make-lc "s" 1) (make-lc "f" 2)))
              (list (make-lc "t" 3) (make-lc "f" 2) (make-lc "s" 1)))
(check-expect (sort (list (make-lc "s" 1) (make-lc "t" 3) (make-lc "f" 2)))
              (list (make-lc "t" 3) (make-lc "f" 2) (make-lc "s" 1)))

(define (sort lcs)
  (cond
    [(empty? lcs) '()]
    [else (insert (first lcs)
                  (sort (rest lcs)))]))

; LetterCount List-of-Lc -> List-of-Lc
(define (insert lc lcs)
  (cond
    [(empty? lcs) (cons lc '())]
    [else (if (more-freq? lc (first lcs))
              (cons lc lcs)
              (cons (first lcs) (insert lc (rest lcs))))]))

; LetterCount LetterCount -> Boolean
(define (more-freq? lc1 lc2)
  (>= (lc-count lc1) (lc-count lc2)))

; List-of-dictionaries is one of:
; - '()
; (cons Dictionary List-of-dictionaries)

; Dictionary -> List-of-dictionaries
; produces list of dics, one per letter
(check-expect (words-by-first-letter '()) '())
(check-expect (words-by-first-letter (list "first" "second" "third" "fourth"))
              (list (list "second") (list "third") (list "first" "fourth")))
(check-expect (words-by-first-letter (list "fifth" "sixth" "seventh" "eighth"))
              (list (list "fifth") (list "sixth" "seventh") (list "eighth")))

(define (words-by-first-letter dic)
  (dictionaries-by-letter (explode (first-letters dic)) dic))

(define (dictionaries-by-letter letters dic)
  (cond
    [(empty? letters) '()]
    [else (cons (create-dic (first letters) dic)
                (dictionaries-by-letter (rest letters) dic))]))

; Letter -> Dictionary
(define (create-dic l dic)
  (cond
    [(empty? dic) '()]
    [else (add-word-if-matching l (first dic)
                                (create-dic l (rest dic)))]))

; String String Dictionary -> Dictionary
(define (add-word-if-matching l w dic)
  (if (string=? l (string-ith w 0))
      (cons w dic)
      dic))

; Dictionary -> Letter-Count
(check-expect (most-frequent.v2 (list "first" "second" "third" "fourth"))
              (make-lc "f" 2))
(define (most-frequent.v2 dic)
  (to-letter-count (biggest-dic (words-by-first-letter dic))))

; Dictionary -> Letter-Count
(define (to-letter-count dic)
  (make-lc (string-ith (first dic) 0)
           (length dic)))

; List-of-dictionaries -> Dictionary
(define (biggest-dic lod)
  (cond
    [(empty? lod) '()]
    [else (compare-dics (first lod)
                        (biggest-dic (rest lod)))]))

(define (compare-dics d1 d2)
  (if (>= (length d1) (length d2)) d1 d2))

;(check-expect
;(most-frequent AS-LIST)
;(most-frequent.v2 AS-LIST))
