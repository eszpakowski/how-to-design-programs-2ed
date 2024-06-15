;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch6.2_4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define EXPECTED-FIRST "expecting 'a'")
(define EXPECTED-AFTER "expecting 'b', 'c', or 'd'")
(define FINISHED "input ended with 'd'")
(define ERROR "incorrect char")

; InputStatus is one of:
; - EXPECTED-FIRST
; - EXPECTED-AFTER
; - FINISHED
; - ERROR

; InputStatus -> InputStatus
(define (check-input is)
  (big-bang is
    [to-draw render]
    [on-key parse]))

; InputStatus -> Image
; renders status of input validation
(check-expect (render EXPECTED-FIRST)
              (rectangle 100 100 "solid" "white"))
(check-expect (render EXPECTED-AFTER)
              (rectangle 100 100 "solid" "yellow"))
(check-expect (render FINISHED)
              (rectangle 100 100 "solid" "green"))
(check-expect (render ERROR)
              (rectangle 100 100 "solid" "red"))

(define (render is)
  (rectangle 100 100 "solid" (cond
                               [(string=? EXPECTED-FIRST is) "white"]
                               [(string=? EXPECTED-AFTER is) "yellow"]
                               [(string=? FINISHED is) "green"]
                               [(string=? ERROR is) "red"])))

; InputStatus KeyEvent -> InputStatus
; parses input and changes state based on its validity
(check-expect (parse EXPECTED-FIRST "b") ERROR)
(check-expect (parse EXPECTED-FIRST "c") ERROR)
(check-expect (parse EXPECTED-FIRST "d") ERROR)
(check-expect (parse EXPECTED-AFTER "b") EXPECTED-AFTER)
(check-expect (parse EXPECTED-AFTER "c") EXPECTED-AFTER)
(check-expect (parse EXPECTED-AFTER "a") ERROR)
(check-expect (parse EXPECTED-AFTER "d") FINISHED)

(define (parse is ke)
  (cond
    [(string=? EXPECTED-FIRST is)
     (parse-first ke)]
    [(string=? EXPECTED-AFTER is)
     (parse-after ke)]
    [else is]))

; KeyEvent -> InputStatus
(define (parse-first ke)
  (if (string=? "a" ke)
      EXPECTED-AFTER ERROR))

; KeyEvent -> InputStatus
(define (parse-after ke)
  (cond
    [(or (string=? "b" ke)
         (string=? "c" ke)) EXPECTED-AFTER]
    [(string=? "d" ke) FINISHED]
    [else ERROR]))

(check-input EXPECTED-FIRST)