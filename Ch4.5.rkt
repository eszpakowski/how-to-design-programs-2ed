;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch4.5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))


; An LRCD (shorthand for launching rocket countdown) is one of:
; - "resting"
; - a Number between -3 and -1 (the countdown to lunch)
; - a NonnegativeNumber
; interpretation: current position of rocket image
; beginning with a the resting position, then the 3 second countdown,
; then with a flying upward state described by a number specifying
; distance in pixels from bottom of the scene to the center of the rocket


; LRCD -> LRCD
; lowers the countdown or
; moves the rocket upwards by some constant
; if its not in resting or countdown state
(check-expect (fly-rocket "resting") "resting")
(check-expect (fly-rocket -3) -2)
(check-expect (fly-rocket 50) (+ 50 YDELTA))

(define (fly-rocket x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (+ x 1)]
    [(>= x 0) (+ x YDELTA)]))


; LRCD -> Image
; renders rocket in a specified position
(check-expect (draw-rocket "resting")
              (place-image ROCKET (/ WIDTH 2) (- HEIGHT CENTER) BACKG))
(check-expect (draw-rocket -3)
              (place-image (text "-3" 20 "red")
                           10 (* 3/4 WIDTH)
                           (place-image ROCKET (/ WIDTH 2) (- HEIGHT CENTER) BACKG)))
(check-expect (draw-rocket 0)
              (place-image ROCKET (/ WIDTH 2) (- HEIGHT CENTER) BACKG))
(check-expect (draw-rocket 20)
              (place-image ROCKET (/ WIDTH 2) (- HEIGHT 20 CENTER) BACKG))
(check-expect (draw-rocket HEIGHT)
              (place-image ROCKET (/ WIDTH 2) (- HEIGHT HEIGHT CENTER) BACKG))

(define (draw-rocket x)
  (cond
    [(string? x) (place-rocket 0)]
    [(<= -3 x -1) (place-rocket-countdown x)]
    [(>= x 0) (place-rocket x)]))

(define (place-rocket x)
  (place-image ROCKET (/ WIDTH 2) (- HEIGHT x CENTER) BACKG))

(define (place-rocket-countdown x)
  (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (place-rocket 0)))


; LRCD KeyEvent -> LRCD
; lunches rocket from resting position
; if space is pressed
(check-expect (lunch-rocket "resting" "a") "resting")
(check-expect (lunch-rocket "resting" " ") -3)
(check-expect (lunch-rocket -3 " ") -3)
(check-expect (lunch-rocket 10 " ") 10)

(define (lunch-rocket x ke)
  (cond
    [(string? x) (if (string=? ke " ") -3 x)]
    [else x]))

; LRCD -> LRCD
(define (main x)
  (big-bang x
    [on-tick fly-rocket 1]
    [to-draw draw-rocket]
    [on-key lunch-rocket]))
