;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch6.1_4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define AUTO "automobile")
(define VAN "van")
(define BUS "bus")
(define SUV "SUV")

; VehicleType is one of the following:
; - AUTO
; - VAN
; - BUS
; - SUV

(define-struct vehicle [type passengers license-plate fuel])
; Vehicle is a structure:
;    (make-vehicle VehicleType Number String Number)
; interpretation representation a vehicle describing
; its type, the number of passengers that it can carry
; its license plate number, and its fuel consumption (miles/gallon)

(define (consume-vehicle v)
  (cond
    [(string=? AUTO (vehicle-type v))
     v];(vehicle-passengers v) (vehicle-license-plate v) (vehicle-fuel v)]
    [(string=? VAN (vehicle-type v))
     v];(vehicle-passengers v) (vehicle-license-plate v) (vehicle-fuel v)]
    [(string=? BUS (vehicle-type v))
     v];(vehicle-passengers v) (vehicle-license-plate v) (vehicle-fuel v)]
    [(string=? SUV (vehicle-type v))
     v]));(vehicle-passengers v) (vehicle-license-plate v) (vehicle-fuel v)]))



; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point
(define N1 -1)
(define N2 -2)
(define P1 1)
(define P2 2)
(define POS1 (make-posn 1 1))
(define POS2 (make-posn 2 2))