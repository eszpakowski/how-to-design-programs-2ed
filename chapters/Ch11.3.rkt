;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ch11.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points

; List-of-players is one of:
; - '()
; - (cons GamePlayer List-of-players)

; List-of-players -> List-of-players
; sorts list of players by score
(check-expect (sort> '()) '())
(check-expect (sort> (list (make-gp "ABC" 1111))) (list (make-gp "ABC" 1111)))
(check-expect (sort> (list (make-gp "ABC" 1111)
                           (make-gp "ABC" 2222)
                           (make-gp "ABC" 3333)))
              (list (make-gp "ABC" 3333)
                    (make-gp "ABC" 2222)
                    (make-gp "ABC" 1111)))
(check-expect (sort> (list (make-gp "ABC" 3333)
                           (make-gp "ABC" 2222)
                           (make-gp "ABC" 1111)))
              (list (make-gp "ABC" 3333)
                    (make-gp "ABC" 2222)
                    (make-gp "ABC" 1111)))

(define (sort> l)
  (cond
    [(empty? l) '()]
    [else (insert (first l) (sort> (rest l)))]))

; GamePlayer List-of-players -> List-of-players
; inserts gp into l at the right place in descending order
; based on score
(define (insert gp l)
  (cond
    [(empty? l) (cons gp '())]
    [else (if (insertion-point? gp (first l))
              (cons gp l)
              (cons (first l) (insert gp (rest l))))]))

; GamePlayer GamePlayer -> Boolean
(define (insertion-point? gp1 gp2)
  (>= (gp-score gp1) (gp-score gp2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time
(define em1 (make-email "Janek" 1577836800 "message contents 1"))
(define em2 (make-email "Kazik" 1577923200 "message contents 2"))
(define em3 (make-email "Janusz" 1578182400 "message contents 3"))

; List-of-emails is one of:
; - '()
; - (cons Email List-of-emails)


; List-of-emails -> List-of-emails
; sorts list of emails by date desc
(check-expect (sort-by-date '()) '())
(check-expect (sort-by-date (list em1 em2 em3))
              (list em1 em2 em3))
(check-expect (sort-by-date (list em2 em3 em1))
              (list em1 em2 em3))
(check-expect (sort-by-date (list em3 em2 em1))
              (list em1 em2 em3))

(define (sort-by-date l)
  (cond
    [(empty? l) '()]
    [else (insert-by-date (first l)
                          (sort-by-date (rest l)))]))

; Email List-of-emails -> List-of-emails
(define (insert-by-date e l)
  (cond
    [(empty? l) (cons e '())]
    [else (if (newer-date e (first l))
              (cons e l)
              (cons (first l) (insert-by-date e (rest l))))]))

; Email Email -> Boolean
(define (newer-date e1 e2)
  (<= (email-date e1) (email-date e2)))


; List-of-emails -> List-of-emails
; sorts list of emails by name desc
(check-expect (sort-by-name '()) '())
(check-expect (sort-by-name (list em1 em3 em2))
              (list em1 em3 em2))
(check-expect (sort-by-name (list em2 em3 em1))
              (list em1 em3 em2))
(check-expect (sort-by-name (list em3 em2 em1))
              (list em1 em3 em2))

(define (sort-by-name l)
  (cond
    [(empty? l) '()]
    [else (insert-by-name (first l)
                          (sort-by-name (rest l)))]))

; Email List-of-emails -> List-of-emails
(define (insert-by-name e l)
  (cond
    [(empty? l) (cons e '())]
    [else (if (compare-name e (first l))
              (cons e l)
              (cons (first l) (insert-by-name e (rest l))))]))

; Email Email -> Boolean
(define (compare-name e1 e2)
  (string<? (email-from e1) (email-from e2)))

;;;;;;;;;

; Number List-of-numbers -> Boolean
; determines whether a number occurs in a sorted list of numbers
(check-expect (search-sorted 1 '())
              #false)
(check-expect (search-sorted 0 (list 1 2 3 4 5))
              #false)
(check-expect (search-sorted 1 (list 1 2 3 4 5))
              #true)
(check-expect (search-sorted 3 (list 1 2 3 4 5))
              #true)
(check-expect (search-sorted 5 (list 1 2 3 4 5))
              #true)
(check-expect (search-sorted 7 (list 1 2 3 4 5))
              #false)

(define (search-sorted n slon)
  (cond
    [(empty? slon) #false]
    [else (cond
            [(= n (first slon)) #true]
            [(< n (first slon)) #false]
            [else (search-sorted n (rest slon))])]))

;;;;;;;;;;;;;;;;;;

; List-of-Lo1S is one of:
; - '()
; - (cons List-of-1Strings List-of-Lo1S)

; List-of-1Strings -> List-of-Lo1S
; consumes a list of 1Strings and produces the list of all prefixes
; a list p is a prefix of l if p and l are the same up through all items in p
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a"))
              (list (list "a")))
(check-expect (prefixes (list "a" "b" "c" "d"))
              (list
               (list "a" "b" "c" "d")
               (list "a" "b" "c")
               (list "a" "b")
               (list "a")))
(check-expect (prefixes (list "x" "y" "z"))
              (list
               (list "x" "y" "z")
               (list "x" "y")
               (list "x")))

(define (prefixes l)
  (cond
    [(empty? l) '()]
    [else
     (cons l (prefixes (drop-last l)))]))

; List-of-1Strings -> List-of-1Strings
(define (drop-last l)
  (cond
    [(empty? (rest l)) '()]
    [else
     (cons (first l)
           (drop-last (rest l)))]))


; 1String List-of-1Strings -> List-of-1Strings
; creates a prefix for 


; List-of-1Strings -> List-of-Lo1S
; consumes a list of 1Strings and produces all suffixes
; a list s is a suffix of l if p and l are the same from the end, up through all items in s
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a"))
              (list (list "a")))
(check-expect (suffixes (list "a" "b" "c" "d"))
              (list
               (list "a" "b" "c" "d")
               (list "b" "c" "d")
               (list "c" "d")
               (list "d")))
(check-expect (suffixes (list "x" "y" "z"))
              (list
               (list "x" "y" "z")
               (list "y" "z")
               (list "z")))

(define (suffixes l)
  (cond
    [(empty? l) '()]
    [else (cons l (suffixes (rest l)))]))

