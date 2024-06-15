;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch9.5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 220) ; distances in terms of pixels 
(define WIDTH 30)
(define XSHOTS (- (/ WIDTH 2) (/ WIDTH 5)))
 
; graphical constants 
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "green"))
(define SHOT (triangle 8 "solid" "black"))

; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired 

; A Shot is a Number.
; interpretation represents the shot's y-coordinate 

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot 

; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))
 
; ShotWorld -> ShotWorld 
; moves each shot up by one pixel
(check-expect (tock (cons 111 (cons 8 (cons 5 '()))))
              (cons 110 (cons 7 (cons 4 '()))))
(check-expect (tock (cons -1 '()))
              '())

(define (tock w)
  (cond
    [(empty? w) '()]
    [else (move-shots (first w) (rest w))]))

; Shot List-of-shots -> ShotWorld
; move all shots in a list recursively
; remove shots that left the scene
(define (move-shots s l)
  (cond
    [(< s 0) (tock l)]
    [else (cons (sub1 s) (tock l))]))
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar is hit
(check-expect (keyh '() "a")
              '())
(check-expect (keyh '() " ")
              (cons HEIGHT '()))

(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))
 
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(check-expect (to-image '()) BACKGROUND)
(check-expect (to-image (cons 111 (cons 8 (cons 5 '()))))
              (place-image SHOT XSHOTS 111
                           (place-image SHOT XSHOTS 8
                                        (place-image SHOT XSHOTS 5 BACKGROUND))))

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

(main (cons 0 '()))