;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ch11.4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)

(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))

(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

; a plain background image 
(define MT (empty-scene 50 50))

; Image Polygon -> Image
; renders the given polygon p into img
(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))
(check-expect
 (connect-dots MT square-p)
 (scene+line
  (scene+line
   (scene+line MT 10 10 20 10 "red")
   20 10 20 20 "red")
  20 20 10 20 "red"))

; Image NELoP -> Image 
; connects the dots in p by rendering lines in img
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) MT]
    [else (render-line
           (connect-dots img (rest p))
           (first p) (second p))]))

; Image Posn Posn -> Image 
; renders a line from p to q into img
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))

; Image Polygon -> Image 
; adds an image of p to img
(check-expect
 (render-poly MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))
(check-expect
 (render-poly MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))

(define (render-poly img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))

(define (render-poly2 img p)
  (connect-dots img (cons (last p) p)))

(define (render-poly3 img p)
  (connect-dots img (add-at-end (first p) p)))

(define (render-poly4 img p)
  (connect-dots2 img (first p) p))

; Posn NELoP -> NELoP
(check-expect (add-at-end
               (make-posn 20 10)
               (list
                (make-posn 20 10)
                (make-posn 20 20)
                (make-posn 30 20)))
              (list
               (make-posn 20 10)
               (make-posn 20 20)
               (make-posn 30 20)
               (make-posn 20 10)))

(define (add-at-end p l)
  (cond
    [(empty? l) (cons p '())]
    [else (cons (first l)
                (add-at-end p (rest l)))]))

; NELoP -> Posn
; extracts the last item from p
(check-expect (last triangle-p)
              (make-posn 30 20))
(check-expect (last square-p)
              (make-posn 10 20))

(define (last p)
  (cond
    [(empty? (rest p)) (first p)]
    [else (last (rest p))]))

; Image Posn NELoP -> Image 
; connects the dots in p by rendering lines in img
(define (connect-dots2 img start l)
  (cond
    [(empty? (rest l)) (render-line MT
                                    (first l) start)]
    [else (render-line
           (connect-dots2 img start (rest l))
           (first l) (second l))]))



