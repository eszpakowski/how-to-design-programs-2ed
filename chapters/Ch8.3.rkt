;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch8.3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; List-of-Strings String -> Boolean
; determines whether the specified string occurs on alos
(check-expect
 (contains? (cons "A" (cons "B"  (cons "C" '()))) "D")
 #false)
(check-expect
 (contains? (cons "A" (cons "B" (cons "C" '()))) "C")
 #true)

(define (contains? alos str)
  (cond
    [(empty? alos) #false]
    [(cons? alos)
     (or (string=? (first alos) str)
         (contains? (rest alos) str))]))

(define alos1
  (cons "Fagan"
        (cons "Findler"
              (cons "Fisler"
                    (cons "Flanagan"
                          (cons "Flatt"
                                (cons "Felleisen"
                                      (cons "Friedman" '()))))))))

(contains? alos1 "Friedman")
(contains? alos1 "123")