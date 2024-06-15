;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch9.3_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

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
(check-expect (draw-hall 8 18)
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

; List-of-Posn is one of:
; - '()
; - (cons Posn List-of-Posn)

; List-of-Posn -> Image
; places red dots on a picture of lecture hall
; based on provided list of positions
(check-expect (add-baloons (cons (make-posn 10 10) '()))
                           (place-image (circle 5 "solid" "red") 10 10 (draw-hall 8 18)))
(check-expect (add-baloons (cons (make-posn 20 10)
                                 (cons (make-posn 10 10) '())))
                           (place-image (circle 5 "solid" "red") 20 10
                                        (place-image (circle 5 "solid" "red") 10 10 (draw-hall 8 18))))
(check-expect (add-baloons (cons (make-posn 30 30)
                                 (cons (make-posn 20 20)
                                       (cons (make-posn 10 10) '()))))
                           (place-image (circle 5 "solid" "red") 30 30
                                        (place-image (circle 5 "solid" "red") 20 20
                                                     (place-image (circle 5 "solid" "red") 10 10 (draw-hall 8 18)))))

(define (add-baloons lop)
  (cond
    [(empty? lop) (draw-hall 8 18)]
    [else (place-image (circle 5 "solid" "red")
                       (posn-x (first lop))
                       (posn-y (first lop))
                       (add-baloons (rest lop)))]))