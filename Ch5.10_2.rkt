;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch5.10_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 20)
(define BCKG (empty-scene WIDTH HEIGHT))
(define TEXT_COLOR "black")
(define TEXT_SIZE 16)
(define COURSOR (rectangle 1 20 "solid" "red"))

(define t1 (overlay/align "left" "center"
                          (beside (text "" TEXT_SIZE TEXT_COLOR)
                                  (beside COURSOR (text "" TEXT_SIZE TEXT_COLOR))) BCKG))
(define t2 (overlay/align "left" "center"
                          (beside (text "hello " TEXT_SIZE TEXT_COLOR)
                                  (beside COURSOR (text "world" TEXT_SIZE TEXT_COLOR))) BCKG))
(define t3 (overlay/align "left" "center"
                          (beside (text "  " TEXT_SIZE TEXT_COLOR)
                                  (beside COURSOR (text "" TEXT_SIZE TEXT_COLOR))) BCKG))

; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor s t) describes an editor
; where s is the visible text and t is
; the number of characters between the first character (counting from the left) and the cursor.
(define-struct editor [str c-pos])

; Editor -> Image
; renders editor based on its state
(check-expect (render (make-editor "" 0)) t1)
(check-expect (render (make-editor "hello world" 6)) t2)
(check-expect (render (make-editor "  " 2)) t3)

(define (render ed)
  (overlay/align "left" "center"
                 (text-with-coursor (editor-str ed) (editor-c-pos ed)) BCKG))

; String String -> Image
; renders text image with a cursor between two strings
(define (text-with-coursor str c-pos)
  (beside (text (substring str 0 c-pos) TEXT_SIZE TEXT_COLOR)
          (beside COURSOR (text (substring str c-pos (string-length str)) TEXT_SIZE TEXT_COLOR))))

; Editor KeyEvent -> Editor
; adds ke to the end of the pre field of ed
; if ke denotes backspace ("\b") key deletes the character to the left of the cursor
; ignores tab and return key
(check-expect (edit (make-editor "" "") "") (make-editor "" ""))
(check-expect (edit (make-editor "hellworld" 4) "o") (make-editor "helloworld" 5))
(check-expect (edit (make-editor "helloworld" 5) " ") (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 6) "right") (make-editor "hello world" 7))
(check-expect (edit (make-editor "hello world" 7) "right") (make-editor "hello world" 8))
(check-expect (edit (make-editor "hello world" 11) "right") (make-editor "hello world" 11))
(check-expect (edit (make-editor "hello world" 8) "left") (make-editor "hello world" 7))
(check-expect (edit (make-editor "hello world" 7) "left") (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 0) "left") (make-editor "hello world" 0))
(check-expect (edit (make-editor "hello world" 8) "\b") (make-editor "hello wrld" 7))
(check-expect (edit (make-editor "hello world" 5) "\t") (make-editor "hello world" 5))
(check-expect (edit (make-editor "hello world" 5) "\r") (make-editor "hello world" 5))

(define (edit ed ke)
  (cond
    [(string=? "left" ke)
     (make-editor (editor-str ed) (coursor-left ed))]
    [(string=? "right" ke)
     (make-editor (editor-str ed) (coursor-right ed))]
    [(string=? "\b" ke)
     (make-editor (remove-char-at (editor-str ed) (editor-c-pos ed)) (coursor-left ed))]
    [(and (= (string-length ke) 1)
          (not (or (string=? "\t" ke) (string=? "\r" ke))))
     (make-editor (add-char-at ke (editor-str ed) (editor-c-pos ed)) (coursor-right ed))]
    [else ed]))

; Number -> Number
; moves cursor right unless position 0
(define (coursor-left ed)
  (if (= (editor-c-pos ed) 0)
      0
      (- (editor-c-pos ed) 1)))

; Number -> Number
; moves cursor right unless poalready at the end of string
(define (coursor-right ed)
  (if (>= (editor-c-pos ed) (string-length (editor-str ed)))
      (editor-c-pos ed)
      (+ (editor-c-pos ed) 1)))

; String -> String
; removes character at a specified position
(define (remove-char-at str pos)
  (cond
    [(or (<= pos 0)
         (> pos (string-length str))) str]
    [else (string-append
           (substring str 0 (- pos 1))
           (substring str pos (string-length str)))]))

; Editor 1String -> Editor
; appends character to the right of ed's pre
(define (add-char-at ke str pos)
  (if (image-width-fits str pos)
      (string-append
           (substring str 0 pos)
           ke
           (substring str pos (string-length str)))
      str))

; Editor -> Boolean
; checks if the current image will fit the backgroud width
(define (image-width-fits str pos)
  (>= WIDTH (image-width (text-with-coursor str pos))))

; Editor -> Editor
; runs the editor
(define (run ed)
  (big-bang ed
    [on-key edit]
    [to-draw render]))

(run (make-editor "Hello World" 6))