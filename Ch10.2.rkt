;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch10.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct work [employee employeeNo rate hours])
; A (piece of) Work is a structure: 
;   (make-work String String Number Number)
; interpretation emloyee and his rates

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

; Low -> List-of-numbers
; computes the weekly wages for all weekly work records 
(check-expect
 (wage*.v2 (cons (make-work "Robby" "1234" 11.95 39) '()))
 (cons (* 11.95 39) '()))

(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (wage.v2 (first an-low))
                          (wage*.v2 (rest an-low)))]))
 
; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

; Lop is one of:
; - '()
; - (cons Paycheck Lop)

(define-struct paycheck [name employeeNo amount])
; Paycheck is a structure:
;     (make-paycheck String String Number)
; interpretation record contaning a paycheck
; for a specific person

; Low -> Lop
; computes the weekly paychecks for all weekly work records
(define (wage*.v4 low)
  (cond
    [(empty? low) '()]
    [else (cons (wage.v4 (first low))
                (wage*.v4 (rest low)))]))

; Work -> Paycheck
; creates paycheck from a work record
(check-expect
 (wage*.v4 (cons (make-work "Robby" "1234" 11.95 39) '()))
 (cons (make-paycheck "Robby" "1234" (* 11.95 39)) '()))

(define (wage.v4 w)
  (make-paycheck (work-employee w)
                 (work-employeeNo w)
                 (* (work-rate w) (work-hours w))))

; Lop is one of:
; - '()
; - (cons Posn Lop)

; Lop -> Number
; sums x coordinates of all positions
(check-expect (sum (cons (make-posn 1 1) '()))
              1)
(check-expect (sum (cons (make-posn 10 20)
                         (cons (make-posn 20 30) '())))
              30)

(define (sum lop)
  (cond
    [(empty? lop) 0]
    [else (+ (posn-x (first lop))
             (sum (rest lop)))]))

; Lop -> Lop
; translation of a list of positions
; each y coordinate is inc by 1
(check-expect (translate (cons (make-posn 10 20) '()))
              (cons (make-posn 10 21) '()))
(check-expect (translate (cons (make-posn 10 20)
                               (cons (make-posn 20 45) '())))
              (cons (make-posn 10 21)
                    (cons (make-posn 20 46) '())))

(define (translate lop)
  (cond
    [(empty? lop) '()]
    [else (cons (translate-pos (first lop))
                (translate (rest lop)))]))

; Posn -> Posn
; translation of a position
; y coordinate is inc by 1
(define (translate-pos p)
  (make-posn (posn-x p) (+ (posn-y p) 1)))

; Lop -> Lop
; filters only positions where
; x-coordinates are between 0 and 100
; and whose y-coordinates are between 0 and 2001
(check-expect (legal (cons (make-posn 10 20) '()))
              (cons (make-posn 10 20) '()))
(check-expect (legal (cons (make-posn 101 20) '()))
              '())
(check-expect (legal (cons (make-posn 10 201) '()))
              '())
(check-expect (legal (cons (make-posn 10 20)
                           (cons (make-posn 20 45) '())))
              (cons (make-posn 10 20)
                    (cons (make-posn 20 45) '())))
(check-expect (legal (cons (make-posn 101 20)
                           (cons (make-posn 20 201) '())))
              '())
(check-expect (legal (cons (make-posn 10 20)
                           (cons (make-posn 101 20)
                                 (cons (make-posn 20 20) '()))))
              (cons (make-posn 10 20)
                    (cons (make-posn 20 20)'())))


(define (legal lop)
  (cond
    [(empty? lop) '()]
    [else (add-legal-only (first lop)
                          (rest lop))]))

(define (add-legal-only pos lop)
  (if (and (< 0 (posn-x pos) 100)
           (< 0 (posn-y pos) 200))
      (cons pos (legal lop))
      (legal lop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define INCORRECT_CODE 713)
(define CORRECT_CODE 281)

(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999. 

; List-of-phones is one of:
; - '()
; - (cons Phone List-of-phones)

; List-of-phones -> List-of-phones
; replaces all occurrence of area INCORRECT_CODE with CORRECT_CODE
(check-expect (replace (cons (make-phone 100 100 999) '()))
              (cons (make-phone 100 100 999) '()))
(check-expect (replace (cons (make-phone INCORRECT_CODE INCORRECT_CODE INCORRECT_CODE) '()))
              (cons (make-phone CORRECT_CODE CORRECT_CODE CORRECT_CODE) '()))

(define (replace lop)
  (cond
    [(empty? lop) '()]
    [else (cons (replace-phone (first lop))
                (replace (rest lop)))]))

; Phone -> Phone
(define (replace-phone p)
  (make-phone
   (replace-code (phone-area p))
   (replace-code (phone-switch p))
   (replace-code (phone-four p))))

; Number -> Number
(define (replace-code n) (if (= INCORRECT_CODE n) CORRECT_CODE n))

