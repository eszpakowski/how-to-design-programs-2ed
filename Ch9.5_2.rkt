;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch9.5_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define CHAIR-SIZE 10)
(define HALL-ROWS 18)
(define HALL-COLUMNS 8)
(define WIDTH (* HALL-COLUMNS CHAIR-SIZE))
(define HEIGHT (* HALL-ROWS CHAIR-SIZE))

; List-of-Posn is one of:
; - '()
; - (cons Posn List-of-Posn)

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lob

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

(define (riot p)
  (big-bang p
    [on-tick throw 1]
    [on-draw add-baloons]))

; N Image -> Image
; creates a row of n images img
(check-expect (row 2 (ellipse 20 30 "solid" "purple"))
              (beside (ellipse 20 30 "solid" "purple") (ellipse 20 30 "solid" "purple")))
(check-expect (row 3 (ellipse 20 30 "solid" "purple"))
              (beside (ellipse 20 30 "solid" "purple") (ellipse 20 30 "solid" "purple") (ellipse 20 30 "solid" "purple")))

(define (row n img)
  (cond
    [(zero? n) empty-image]
    [else (beside img (row (sub1 n) img))]))

; N Image -> Image
; creates a column of n images img
(check-expect (column 2 (ellipse 20 30 "solid" "purple"))
              (above (ellipse 20 30 "solid" "purple") (ellipse 20 30 "solid" "purple")))
(check-expect (column 3 (ellipse 20 30 "solid" "purple"))
              (above (ellipse 20 30 "solid" "purple") (ellipse 20 30 "solid" "purple") (ellipse 20 30 "solid" "purple")))

(define (column n img)
  (cond
    [(zero? n) empty-image]
    [else (above img (column (sub1 n) img))]))

; N N -> Image
; draws a lecture hall
(check-expect (draw-hall HALL-COLUMNS HALL-ROWS)
              (above (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))
                     (beside (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black") (square 10 "outline" "black"))))

(define (draw-hall n m)
  (column m (row n (square 10 "outline" "black"))))

; Pair -> Image
; places red dots on a picture of lecture hall
; based on provided list of positions
(check-expect (add-baloons (make-pair 10 (cons (make-posn 10 10) '())))
              (place-image (circle 5 "solid" "red") 10 10 (draw-hall HALL-COLUMNS HALL-ROWS)))
(check-expect (add-baloons (make-pair 10 (cons (make-posn 20 10)
                                               (cons (make-posn 10 10) '()))))
              (place-image (circle 5 "solid" "red") 20 10
                           (place-image (circle 5 "solid" "red") 10 10 (draw-hall HALL-COLUMNS HALL-ROWS))))
(check-expect (add-baloons (make-pair 10 (cons (make-posn 30 30)
                                               (cons (make-posn 20 20)
                                                     (cons (make-posn 10 10) '())))))
              (place-image (circle 5 "solid" "red") 30 30
                           (place-image (circle 5 "solid" "red") 20 20
                                        (place-image (circle 5 "solid" "red") 10 10 (draw-hall HALL-COLUMNS HALL-ROWS)))))

(define (add-baloons p)
  (cond
    [(empty? (pair-lob p)) (draw-hall HALL-COLUMNS HALL-ROWS)]
    [else (place-image (circle 5 "solid" "red")
                       (posn-x (first (pair-lob p)))
                       (posn-y (first (pair-lob p)))
                       (add-baloons (make-pair (pair-balloon# p) (rest (pair-lob p)))))]))

; Pair -> Pair
; thows a baloon in a random place
(define (throw p)
  (throw-deterministic p (make-posn (random WIDTH) (random HEIGHT))))

; Pair Posn -> Pair
; throws a baloon in the specified place
(check-expect (throw-deterministic (make-pair 10 (cons (make-posn 10 10) '()))
                                   (make-posn 2 2))
              (make-pair 9 (cons (make-posn 2 2) (cons (make-posn 10 10) '()))))

(define (throw-deterministic p pos)
  (cond
    [(= (pair-balloon# p) 0) p]
    [else (make-pair (- (pair-balloon# p) 1)
                     (cons pos (pair-lob p)))]))

(riot (make-pair 100 '()))