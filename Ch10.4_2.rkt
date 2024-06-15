;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch10.4_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))


(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)

; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

; Lo1s -> Lo1s 
; produces a reverse version of the given list 
(check-expect
 (rev (cons "a" (cons "b" (cons "c" '()))))
 (cons "c" (cons "b" (cons "a" '()))))
 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else
     (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
(check-expect
 (add-at-end (cons "c" (cons "b" '())) "a")
 (cons "c" (cons "b" (cons "a" '()))))

(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l)
                (add-at-end (rest l) s))]))

; String String -> Editor
; creates editor with text to the left and the
; right of the cursor
(check-expect (create-editor "Hello " "World")
              (make-editor (rev (explode "Hello ")) (explode "World")))
(define (create-editor left right)
  (make-editor (rev (explode left)) (explode right)))

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e) MT)
 
; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "e")
 (create-editor "cde" "fgh"))
(check-expect
 (editor-kh (create-editor "" "") "\b")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "\b")
 (create-editor "c" "fgh"))
(check-expect
 (editor-kh (create-editor "" "") " ")
 (create-editor " " ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") " ")
 (create-editor "cd " "fgh"))
(check-expect
 (editor-kh (create-editor "" "") "left")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "left")
 (create-editor "c" "dfgh"))
(check-expect
 (editor-kh (create-editor "" "") "right")
 (create-editor "" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "right")
 (create-editor "cdf" "gh"))
(check-expect
 (editor-kh (create-editor "cdfgh" "") "right")
 (create-editor "cdfgh" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "down")
 (create-editor "cd" "fgh"))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; Editor -> Editor
; moves cursor left (post to pre)
(check-expect
 (editor-lft
  (make-editor (cons "d" '())
               (cons "f" (cons "g" '()))))
 (make-editor (cons "" '())
              (cons "d" (cons "f" (cons "g" '())))))

(define (editor-lft ed)
  (make-editor (move-1str (editor-post ed) (editor-pre ed))
               (rest-1strs (editor-post ed))))

; Editor -> Editor
; moves cursor right (pre to post)
(define (editor-rgt ed)
  (make-editor (rest-1strs (editor-pre ed))
               (move-1str (editor-pre ed) (editor-post ed))))

; Editor -> Editor
; defetes character before cursor
(define (editor-del ed)
  (make-editor (rest-1strs (editor-pre ed))
               (editor-post ed)))

; Lo1S Lo1S -> Lo1S
(define (move-1str l1 l2)
  (cond
    [(empty? l1) l2]
    [else (cons (first l1) l2)]))

; Lo1S -> Lo1S
(define (rest-1strs l)
  (cond
    [(empty? l) '()]
    [else (rest l)]))

; Editor 1String -> Editor
; insert the 1String k between pre and post
(check-expect
 (editor-ins (make-editor '() '()) "e")
 (make-editor (cons "e" '()) '()))
(check-expect
 (editor-ins
  (make-editor (cons "d" '())
               (cons "f" (cons "g" '())))
  "e")
 (make-editor (cons "e" (cons "d" '()))
              (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

