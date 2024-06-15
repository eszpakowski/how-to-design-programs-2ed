;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch6.1_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200) ; distances in terms of pixels 
(define HEIGHT 300)
(define CLOSE (/ HEIGHT 3))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define UFO-WIDTH 40)
(define UFO (overlay/offset (circle 10 "solid" "green")
                            0 10
                            (ellipse UFO-WIDTH 20 "solid" "gray")))

(define TANK-HEIGHT 10)
(define TANK-WIDTH 30)
(define TANK-VEL -3)
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "black"))
(define MISSILE-HEIGHT 10)
(define MISSILE (rectangle 3 MISSILE-HEIGHT "solid" "black"))

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game
(make-sigs (make-posn 20 10)
           (make-tank 28 -3)
           (make-posn 28 (- HEIGHT TANK-HEIGHT)))
(make-sigs (make-posn 20 100)
           (make-tank 100 3)
           (make-posn 22 103))
 
; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location

; SIGS.v2 -> Image
; adds TANK, UFO, and possibly MISSILE to the BACKGROUND scene
(check-expect (si-render (make-sigs (make-posn 10 20) (make-tank 28 -3) #false))
              (place-image UFO
                           10 20
                           (place-image TANK 28 (- HEIGHT TANK-HEIGHT) BACKGROUND)))
(check-expect (si-render (make-sigs (make-posn 20 100) (make-tank 100 3) (make-posn 22 103)))
              (place-image UFO
                           20 100
                           (place-image MISSILE
                                        22 103
                                        (place-image TANK 100 (- HEIGHT TANK-HEIGHT) BACKGROUND))))
(define (si-render s)
  (cond
    [(boolean? (sigs-missile s))
     (ufo-render (sigs-ufo s)
                 (tank-render (sigs-tank s) BACKGROUND))]
    [(posn? (sigs-missile s))
     (ufo-render (sigs-ufo s)
                 (missile-render (sigs-missile s)
                                 (tank-render (sigs-tank s) BACKGROUND)))]))
; Ufo Image -> Image
; renders the ufo on background img
(check-expect (ufo-render (make-posn 10 20) BACKGROUND)
              (place-image UFO 10 20 BACKGROUND))
(check-expect (ufo-render (make-posn 20 10) BACKGROUND)
              (place-image UFO 20 10 BACKGROUND))
(check-expect (ufo-render (make-posn 20 100) BACKGROUND)
              (place-image UFO 20 100 BACKGROUND))

(define (ufo-render u img)
  (place-image UFO (posn-x u) (posn-y u) img))

; Tank Image -> Image
; renders the tank on background img
(check-expect (tank-render (make-tank 28 -3) BACKGROUND)
              (place-image TANK 28 (- HEIGHT TANK-HEIGHT) BACKGROUND))
(check-expect (tank-render (make-tank 5 -3) BACKGROUND)
              (place-image TANK 5 (- HEIGHT TANK-HEIGHT) BACKGROUND))
(check-expect (tank-render (make-tank 100 3) BACKGROUND)
              (place-image TANK 100 (- HEIGHT TANK-HEIGHT) BACKGROUND))

(define (tank-render t img)
  (place-image TANK (tank-loc t) (- HEIGHT TANK-HEIGHT) img))

; MissileOrNot Image -> Image
; renders the missile on background img
(check-expect (missile-render #false BACKGROUND) BACKGROUND)
(check-expect (missile-render (make-posn 28 (- HEIGHT TANK-HEIGHT MISSILE-HEIGHT)) BACKGROUND)
              (place-image MISSILE 28 (- HEIGHT TANK-HEIGHT MISSILE-HEIGHT) BACKGROUND))
(check-expect (missile-render (make-posn 22 103) BACKGROUND)
              (place-image MISSILE 22 103 BACKGROUND))

(define (missile-render m img)
  (cond
    [(boolean? m) img]
    [(posn? m)
     (place-image MISSILE (posn-x m) (posn-y m) img)]))

; SIGS.v2 -> Boolean
; stops the game when ufo lands or the missile hits the ufo
(check-expect (si-game-over (make-sigs (make-posn 10 10) (make-tank 28 -3)
                                       #false))
              #false)
(check-expect (si-game-over (make-sigs (make-posn 10 HEIGHT) (make-tank 28 -3)
                                       #false))
              #true)
(check-expect (si-game-over (make-sigs (make-posn 20 HEIGHT) (make-tank 28 -3)
                                       (make-posn 28 (- HEIGHT TANK-HEIGHT))))
              #true)
(check-expect (si-game-over (make-sigs (make-posn 20 10) (make-tank 100 3)
                                       (make-posn 22 10)))
              #true)

(define (si-game-over s)
  (cond
    [(boolean? (sigs-missile s))
     (ufo-landed (sigs-ufo s))]
    [(posn? (sigs-missile s))
     (or (ufo-landed (sigs-ufo s))
         (ufo-hit (sigs-ufo s) (sigs-missile s)))]))

; Ufo -> Boolean
; checks if ufo is close enough (3 pixels) to land
(check-expect (ufo-landed (make-posn 10 10)) #false)
(check-expect (ufo-landed (make-posn 10 (- HEIGHT 4))) #false)
(check-expect (ufo-landed (make-posn 10 (- HEIGHT 3))) #true)

(define (ufo-landed s)
  (>= (posn-y s) (- HEIGHT 3)))

; Ufo Missile -> Boolean
; checks if ufo is close enough to be hit by missile
(check-expect (ufo-hit (make-posn 20 10) (make-posn 28 (- HEIGHT TANK-HEIGHT MISSILE-HEIGHT))) #false)
(check-expect (ufo-hit (make-posn 20 100) (make-posn 44 204)) #false)
(check-expect (ufo-hit (make-posn 20 100) (make-posn 23 103)) #true)
(check-expect (ufo-hit (make-posn 20 100) (make-posn 17 97)) #true)
(check-expect (ufo-hit (make-posn 20 100) (make-posn 20 103)) #true)
(check-expect (ufo-hit (make-posn 20 100) (make-posn 23 100)) #true)

(define (ufo-hit s m)
  (and (<= (abs (- (posn-x s) (posn-x m))) 15)
       (<= (abs (- (posn-y s) (posn-y m))) 15)))

; SIGS.v2 -> SIGS.v2
; move objects in the world state
; moves the ufo randomly
(define (si-move s)
  (si-move-proper s (random 5)))
 
; SIGS.v2 Number -> SIGS.v2
; moves the ufo predictably by delta
(check-expect (si-move-proper (make-sigs (make-posn 10 20) (make-tank 28 -3) #false) 2)
              (make-sigs (make-posn 12 21) (make-tank 25 -3) #false))
(check-expect (si-move-proper (make-sigs (make-posn 20 10) (make-tank 28 -3) #false) 2)
              (make-sigs (make-posn 22 11) (make-tank 25 -3) #false))

(define (si-move-proper s delta)
  (cond
    [(boolean? (sigs-missile s))
     (make-sigs (move-ufo (sigs-ufo s) delta)
                (move-tank (sigs-tank s))
                #false)]
    [(posn? (sigs-missile s))
     (make-sigs (move-ufo (sigs-ufo s) delta)
                (move-tank (sigs-tank s))
                (move-missile (sigs-missile s)))]))

; Ufo Number -> Ufo
; moves ufo downward and randomly left or right
(check-expect (move-ufo (make-posn 10 20) 2)
              (make-posn 12 21))
(check-expect (move-ufo (make-posn 10 20) 3)
              (make-posn 7 21))
(check-expect (move-ufo (make-posn 10 20) 5)
              (make-posn 5 21))

(define (move-ufo u n)
  (cond
    [(>= (posn-x u) WIDTH)
     (make-posn (- (posn-x u) 5) (+ (posn-y u) 1))]
    [(<= (posn-x u) 0)
     (make-posn (+ (posn-x u) 5) (+ (posn-y u) 1))]
    [(odd? n)
     (make-posn (- (posn-x u) n) (+ (posn-y u) 1))]
    [(even? n)
     (make-posn (+ (posn-x u) n) (+ (posn-y u) 1))]))

; Tank -> Tank
; moves tank left or right and by specific
; distance in pixels based on velocity
(check-expect (move-tank (make-tank 28 -3)) (make-tank 25 -3))
(check-expect (move-tank (make-tank 28 3)) (make-tank 31 3))

(define (move-tank t)
  (cond
    [(>= (tank-loc t) WIDTH)
     (make-tank (+ (tank-loc t) (- (tank-vel t))) (- (tank-vel t)))]
    [(<= (tank-loc t) 0)
     (make-tank (+ (tank-loc t) (abs (tank-vel t))) (abs (tank-vel t)))]
    [else
     (make-tank (+ (tank-loc t) (tank-vel t)) (tank-vel t))]))
  
; Missile -> Missile
; moves Missile upwards
(check-expect (move-missile (make-posn 10 20)) (make-posn 10 16))
(check-expect (move-missile (make-posn 10 44)) (make-posn 10 40))

(define (move-missile t)
  (make-posn (posn-x t) (- (posn-y t) 4)))

; SIGS.v2 KeyEvent -> SIGS.v2
; key event handler
(check-expect (si-control (make-sigs (make-posn 10 20) (make-tank 28 -3) #false) "left")
              (make-sigs (make-posn 10 20) (make-tank 8 -3) #false))
(check-expect (si-control (make-sigs (make-posn 10 20) (make-tank 28 -3) #false) "right")
              (make-sigs (make-posn 10 20) (make-tank 48 -3) #false))
(check-expect (si-control (make-sigs (make-posn 10 20) (make-tank 28 -3) #false) " ")
              (make-sigs (make-posn 10 20) (make-tank 28 -3) (make-posn 28 (- HEIGHT TANK-HEIGHT MISSILE-HEIGHT))))
(check-expect (si-control (make-sigs (make-posn 10 20) (make-tank 28 -3) (make-posn 28 (- HEIGHT TANK-HEIGHT MISSILE-HEIGHT))) "left")
              (make-sigs (make-posn 10 20) (make-tank 8 -3) (make-posn 28 (- HEIGHT TANK-HEIGHT MISSILE-HEIGHT))))

(define (si-control s ke)
  (cond
    [(and (boolean? (sigs-missile s))
          (string=? " " ke))
     (make-sigs (sigs-ufo s) (sigs-tank s) (shoot-missile (sigs-tank s)))]
    [(posn? (sigs-missile s))
     (make-sigs (sigs-ufo s) (control-tank (sigs-tank s) ke) (sigs-missile s))]
    [else
     (make-sigs (sigs-ufo s) (control-tank (sigs-tank s) ke) #false)]))

; Tank -> Posn
; creates missile in the initial position
(define (shoot-missile t)
  (make-posn (tank-loc t) (- HEIGHT TANK-HEIGHT MISSILE-HEIGHT)))

; Tank KeyEvent -> Tank
; controls tank movement
(define (control-tank t ke)
  (cond
    [(string=? "left" ke) (make-tank (- (tank-loc t) 20) (tank-vel t))]
    [(string=? "right" ke) (make-tank (+ (tank-loc t) 20) (tank-vel t))]))

; SIGS.v2 -> SIGS.v2
(define (si-main s)
  (big-bang s
    [on-tick si-move]
    [to-draw si-render]
    [on-key si-control]
    [stop-when si-game-over]))

(si-main (make-sigs (make-posn (/ WIDTH 2) 20) (make-tank 28 TANK-VEL) #false))