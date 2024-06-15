;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ch11.1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

(check-expect (list "a" "b" "c" "d")
              (cons "a" (cons "b" (cons "c" (cons "d" '())))))

(check-expect (list (list 1 2))
              (cons (cons 1 (cons 2 '())) '()))
              
(check-expect (list "a" (list 1) #false)
              (cons "a" (cons (cons 1 '()) (cons #false '()))))
              
(check-expect (list (list "a" 2) "hello")
              (cons (cons "a" (cons 2 '())) (cons "hello" '())))
              
(check-expect (list (list 1 2) (list 2))
              (cons (cons 1 (cons 2 '()))
                    (cons (cons 2 '())
                          '())))

(check-expect (list 0 1 2 3 4 5)
              (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))

(check-expect (list (list "he" 0) (list "it" 1) (list "lui" 14))
              (cons (cons "he" (cons 0 '())) (cons (cons "it" (cons 1 '())) (cons (cons "lui" (cons 14 '())) '()))))

(check-expect (list 1 (list 1 2) (list 1 2 3))
              (cons 1 (cons (cons 1 (cons 2 '())) (cons (cons 1 (cons 2 (cons 3 '()))) '()))))

(check-expect (cons "a" (list 0 #false))
              (list "a" 0 #false))

(check-expect (list (cons 1 (cons 13 '())))
              (list (list 1 13)))


(check-expect (cons (list 1 (list 13 '())) '())
              (list (list 1 (list 13 (list)))))

(check-expect (list '() '() (cons 1 '()))
              (list (list) (list) (list 1)))

(check-expect (cons "a" (cons (list 1) (list #false '())))
              (list "a" (list 1) #false (list)))


(check-expect (list (string=? "a" "b") #false)
              (list #false #false))

(check-expect (list (+ 10 20) (* 10 20) (/ 10 20))
              (list 30 200 0.5))

(check-expect (list "dana" "jane" "mary" "laura")
              (list "dana" "jane" "mary" "laura"))

(first (list 1 2 3))
(rest (list 1 2 3))
(second (list 1 2 3))
(third (list 1 2 3))
(fourth (list 1 2 3 4))

