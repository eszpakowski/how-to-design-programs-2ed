;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch10.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)


; An LN is one of: 
; – '()
; – (cons Los LN)
; interpretation a list of lines, each is a list of Strings
 
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define ln0 '())
(define ln1 (cons line0 (cons line1 '())))
 
; LN -> List-of-numbers
; determines the number of words on each line 

(check-expect (words-on-line ln1) (cons 2 (cons 0 '())))
 
(define (words-on-line ln)
  (cond
    [(empty? ln) '()]
    [else (cons (length (first ln))
                (words-on-line (rest ln)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
   (read-words/line file-name)))

; LN -> String
; collapses list of lines into a single string
(check-expect (collapse ln0) "")
(check-expect (collapse ln1) "helloworld")

(define (collapse ln)
  (cond
    [(empty? ln) ""]
    [else (string-append (collapse-line (first ln))
                         (collapse (rest ln)))]))

; List-of-string -> String
; collapses list of strings into a single string
(check-expect (collapse-line line0) "helloworld")

(define (collapse-line los)
  (cond
    [(empty? los) ""]
    [else (string-append (first los)
                         (collapse-line (rest los)))]))


(write-file "ttt.dat"
            (collapse (read-words/line "ttt.txt")))

;;;;;;;;;;;;;;;;;;;;;
(define ARTICLES (cons "a" (cons "an" (cons "the" '()))))

; String -> String
(define (remove-articles-file filename)
  (write-file (string-append "no-articles-" filename)
              (remove-articles (read-words/line filename))))

; LN -> String
; removes all articles from a list of lines
(check-expect (remove-articles ln0) "")
(check-expect (remove-articles ln1) "helloworld")
(check-expect (remove-articles (cons
                                (cons "a" (cons "hello" (cons "world" '()))) '()))
              "helloworld")

(define (remove-articles ln)
  (cond
    [(empty? ln) ""]
    [else (string-append (remove-articles-line (first ln))
                         (remove-articles (rest ln)))]))

; List-of-string -> String
(define (remove-articles-line los)
  (cond
    [(empty? los) ""]
    [else (string-append (remove-articles-string (first los))
                         (remove-articles-line (rest los)))]))

; String -> String
(define (remove-articles-string s)
  (if (member? s ARTICLES) "" s))

(remove-articles-file "ttt.txt")

;;;;;;;;;;;;;;;;;;;;;

; String -> String
; encodes text file numerically
(define (encode-file filename)
  (write-file (string-append "encoded-" filename)
              (encode (read-words/line filename))))

; LN -> String
; transforms list of lines to a single string encoded numerically
(check-expect (encode (cons
                       (cons "a" (cons "bc" '())) '()))
              (string-append "0" (code1 "a")
                             "0" (code1 "b")
                             "0" (code1 "c")))

(define (encode ln)
  (cond
    [(empty? ln) ""]
    [else (string-append (encode-line (first ln))
                         (encode (rest ln)))]))

; List-of-string -> String
(define (encode-line los)
  (cond
    [(empty? los) ""]
    [else (string-append (encode-letters (explode (first los)))
                         (encode-line (rest los)))]))

; List-of-string -> String
(define (encode-letters loc)
  (cond
    [(empty? loc) ""]
    [else (string-append (encode-letter (first loc))
                         (encode-letters (rest loc)))]))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))

(encode-file "ttt.txt")

;;;;;;;;;;;;;;;;;;;;;

(define-struct word-count [c w l])
; WordCount is a structure:
;     (make-wc Number Number Number)
; interpretation: the number of 1Strings, words, and lines in a given file

; String -> WordCount
; count the number of 1Strings, words, and lines in a given file.
(define (wc filename)
  (make-word-count (length (read-1strings filename))
                   (length (read-words filename))
                   (length (read-words/line filename))))

(wc "ttt.txt")

;;;;;;;;;;;;;;;

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

(define r1 (cons 11 (cons 12 '())))
(define r2 (cons 21 (cons 22 '())))

(define m1 (cons r1 (cons r2 '())))


; Matrix -> Matrix
; transposes the given matrix along the diagonal 
 
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
 
(check-expect (transpose mat1) tam1)
 
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

(transpose m1)









