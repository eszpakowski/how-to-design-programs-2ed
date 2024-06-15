;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ch12.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
(define w1 (list "r" "a" "t"))
(define w2 (list "a" "r" "t"))
(define w3 (list "t" "a" "r"))
(define w4 (list "a" "t" "r"))

; List-of-words is one of:
; - '()
; - (cons Word List-of-words)
(define low1 (list w1 w2 w3 w4))

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; List-of-strings -> Boolean 
(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

; String -> List-of-strings
; finds all words that use the same letters as s
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)

(define (alternative-words s)
  (in-dictionary
   (words->strings
    (arrangements (string->word s)))))

; List-of-strings -> List-of-strings
(check-expect (in-dictionary (list "rat" "art" "atr"))
              (list "rat" "art"))

(define (in-dictionary l)
  (cond
    [(empty? l) '()]
    [else
     (add-if-member (first l)
                    (in-dictionary (rest l)))]))

; String List-of-strings
(define (add-if-member s l)
  (if (member? s AS-LIST)
      (cons s l)
      l))

; List-of-words -> List-of-strings
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))

; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "rat")
              (list "r" "a" "t"))

(define (string->word s)
  (explode s))
 
; Word -> String
; converts w to a string
(check-expect (word->string (list "r" "a" "t"))
              "rat")

(define (word->string w)
  (implode w))

; Word -> List-of-words
; produces list of words letter-by-letter rearrangements
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))

; 1String List-of-words -> List-of-words
; inserts a 1String into all possible combinations of the specified word list
(check-expect (insert-everywhere/in-all-words "d" '())
              '())
(check-expect (insert-everywhere/in-all-words "d" (list (list "r" "e") (list "e" "r")))
              (list (list "d" "r" "e") (list "r" "d" "e") (list "e" "r" "d")
                    (list "d" "e" "r") (list "e" "d" "r") (list "r" "e" "d")))
(check-expect (insert-everywhere/in-all-words "c" (list (list "a" "t") (list "t" "a")))
              (list (list "c" "a" "t") (list "a" "c" "t") (list "t" "a" "c")
                    (list "c" "t" "a") (list "t" "c" "a") (list "a" "t" "c")))

(define (insert-everywhere/in-all-words s l)
  (cond
    [(empty? l) '()]
    [else (append (insert-everywhere/in-word '() s (first l))
                  (insert-everywhere/in-all-words s (rest l)))]))

; Word 1String Word -> List-of-words
(check-expect (insert-everywhere/in-word '() "d" '())
              (list (list "d")))
(check-expect (insert-everywhere/in-word '() "d" (list "r" "e"))
              (list (list "d" "r" "e") (list "r" "d" "e") (list "e" "r" "d")))
(check-expect (insert-everywhere/in-word '() "c" (list "a" "t"))
              (list (list "c" "a" "t") (list "a" "c" "t") (list "t" "a" "c")))

(define (insert-everywhere/in-word before s after)
  (cond
    [(empty? after) (list (create-word before  s '()))]
    [else
     (cons (create-word before s after)
           (insert-everywhere/in-word (cons (first after) before) s (rest after)))]))

; Word 1String Word -> Word
(check-expect (create-word (list "r") "e" (list "d"))
              (list "r" "e" "d"))
 
(define (create-word before s after)
  (append before (cons s after)))
