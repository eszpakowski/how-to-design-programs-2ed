;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Intermezzo2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/web-io)

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))
 
; Number -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ,(number->string n)))

(check-expect `(0 ,@'(1 2 3) 4)
              (list 0 1 2 3 4))

(check-expect `(("alan" ,(* 2 500))
                ("barb" 2000)
                (,@'("carl" " , the great")   1500)
                ("dawn" 2300))
              (list (list "alan" (* 2 500))
                    (list "barb" 2000)
                    (list "carl" " , the great" 1500)
                    (list "dawn" 2300)))

(check-expect `(html
                (body
                 (table ((border "1"))
                        (tr ((width "200"))
                            ,@(make-row '( 1  2)))
                        (tr ((width "200"))
                            ,@(make-row '(99 65))))))
              (list 'html
                    (list 'body
                          (list 'table (list (list 'border "1"))
                                (list 'tr (list (list 'width "200"))
                                      (list 'td "1") (list 'td "2"))
                                (list 'tr (list (list 'width "200"))
                                      (list 'td "99") (list 'td "65"))))))

;;;;;;;;;;;;;;;;;;;;;

; Html-table
; nested list representing an html page


; Html-row
; nested list representing an html row with columns 

; LRows is one of:
; - '()
; - (cons Html-row LRows)


; Ranking is a list of two items
; - (cons Number (cons String '()))

; LRanking is one of:
; - '()
; - (cons Ranking LRanking)

(define TABLE-BORDER "1")
(define ROW-WIDTH "200")

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-strings -> Html-table
; creates html table with rankings
(check-expect (make-ranking one-list)
              `(html
                (body
                 (table ((border ,TABLE-BORDER))
                        (tr ((width ,ROW-WIDTH))
                            (td "1")  (td "Asia: Heat of the Moment"))
                        (tr ((width ,ROW-WIDTH))
                            (td "2") (td "U2: One"))
                        (tr ((width ,ROW-WIDTH))
                            (td "3") (td "The White Stripes: Seven Nation Army"))))))

(define (make-ranking los)
  `(html
    (body
     (table ((border ,TABLE-BORDER))
            ,@(create-rows (ranking los))))))

; LRanking -> LRows
; creates html rows based on ranking list
(check-expect (create-rows '((1 "Asia: Heat of the Moment")
                             (2 "U2: One")
                             (3 "The White Stripes: Seven Nation Army")))
              `((tr ((width ,ROW-WIDTH))
                    (td "1") (td "Asia: Heat of the Moment"))
                (tr ((width ,ROW-WIDTH))
                    (td "2") (td "U2: One"))
                (tr ((width ,ROW-WIDTH))
                    (td "3") (td "The White Stripes: Seven Nation Army"))))

(define (create-rows lr)
  (cond
    [(empty? lr) '()]
    [else
     (cons (create-row (first lr))
           (create-rows (rest lr)))]))

; Ranking -> Html-row
; creates a single html row
(check-expect (create-row '(1 "Asia: Heat of the Moment"))
              `(tr ((width ,ROW-WIDTH))
                   (td "1") (td "Asia: Heat of the Moment")))

(define (create-row r)
  `(tr ((width ,ROW-WIDTH))
       (td ,(number->string (first r))) (td ,(second r))))

; List-of-strings -> LRanking
; adds index before each item in a list of strings
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> LRanking
; adds current length of list before each item in a list of strings
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

