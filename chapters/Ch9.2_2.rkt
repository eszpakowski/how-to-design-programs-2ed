;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch9.2_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define rectangle1 (rectangle 6 6 "solid" "red"))
(define rectangle2 (rectangle 6 7 "solid" "red"))
(define rectangle3 (rectangle 20 30 "solid" "red"))

; List-of-Images is one of:
; - '()
; - (cons Image List-of-Images)

; ImageOrFalse is one of:
; - Image
; - Boolean

; List-of-Images PositiveNumber -> ImageOrFalse
; produces the first image on loi that is not an n by n square
; if it cannot find such an image produces #false
(check-expect (ill-sized? '() 6) #false)
(check-expect (ill-sized? (cons rectangle1 '()) 6) #false)
(check-expect (ill-sized? (cons rectangle2 '()) 6) rectangle2)
(check-expect (ill-sized? (cons rectangle1
                                (cons rectangle1 '())) 6)
              #false)
(check-expect (ill-sized? (cons rectangle1
                                (cons rectangle1
                                      (cons rectangle3 '()))) 6)
              rectangle3)
(check-expect (ill-sized? (cons rectangle1
                                (cons rectangle2
                                      (cons rectangle3 '()))) 6)
              rectangle2)

(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (if (image? (ill-sized-img? (first loi) n))
              (first loi)
              (ill-sized? (rest loi) n))]))

; Image Number -> ImageOrFalse
; returns the specified img if its size is NOT n by n pixels
; otherwise returns #false
(define (ill-sized-img? img n)
  (if (not
       (and (= (image-height img) n)
            (= (image-width img) n)))
      img
      #false))