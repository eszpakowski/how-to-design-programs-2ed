;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ch5.10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t
(define-struct editor [pre post])

; Editor -> Image
; renders editor based on its state
(check-expect (render (make-editor "" "")) t1)
(check-expect (render (make-editor "hello " "world")) t2)
(check-expect (render (make-editor "  " "")) t3)

(define (render e)
  (overlay/align "left" "center"
                 (text-with-coursor (editor-pre e) (editor-post e)) BCKG))

; String String -> Image
; renders text image with a cursor between two strings
(define (text-with-coursor t1 t2)
  (beside (text t1 TEXT_SIZE TEXT_COLOR)
          (beside COURSOR (text t2 TEXT_SIZE TEXT_COLOR))))

; Editor KeyEvent -> Editor
; adds ke to the end of the pre field of ed
; if ke denotes backspace ("\b") key deletes the character to the left of the cursor
; ignores tab and return key
(check-expect (edit (make-editor "" "") "") (make-editor "" ""))
(check-expect (edit (make-editor "hell" "world") "o") (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") " ") (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello " "world") "right") (make-editor "hello w" "orld"))
(check-expect (edit (make-editor "hello w" "orld") "right") (make-editor "hello wo" "rld"))
(check-expect (edit (make-editor "hello world" "") "right") (make-editor "hello world" ""))
(check-expect (edit (make-editor "hello wo" "rld") "left") (make-editor "hello w" "orld"))
(check-expect (edit (make-editor "hello w" "orld") "left") (make-editor "hello " "world"))
(check-expect (edit (make-editor "" "hello world") "left") (make-editor "" "hello world"))
(check-expect (edit (make-editor "hello wo" "rld") "\b") (make-editor "hello w" "rld"))
(check-expect (edit (make-editor "hello" "world") "\t") (make-editor "hello" "world"))
(check-expect (edit (make-editor "hello" "world") "\r") (make-editor "hello" "world"))

(define (edit ed ke)
  (cond
    [(string=? "left" ke)
     (move-coursor-left ed)]
    [(string=? "right" ke)
     (move-coursor-right ed)]
    [(string=? "\b" ke)
     (make-editor (remove-last-char (editor-pre ed)) (editor-post ed))]
    [(and (= (string-length ke) 1)
          (not (or (string=? "\t" ke) (string=? "\r" ke))))
     (append-pre ed ke)]
    [else ed]))

; Editor -> Editor
; moves last character from ed's pre to post
(define (move-coursor-left ed)
  (if (= (string-length (editor-pre ed)) 0)
       ed
       (make-editor (remove-last-char (editor-pre ed))
                    (string-append (last-char (editor-pre ed))
                                   (editor-post ed)))))

; Editor -> Editor
; moves first character from ed's post to pre
(define (move-coursor-right ed)
  (if (= (string-length (editor-post ed)) 0)
       ed
       (make-editor (string-append (editor-pre ed)
                                   (first-char (editor-post ed)))
                    (remove-first-char (editor-post ed)))))

; String -> String
; removes first character from a string
(define (remove-first-char s)
  (substring s 1 (string-length s)))

; String -> String
; removes last character from a string
(define (remove-last-char s)
  (if (= (string-length s) 0)
      s
      (substring s 0 (- (string-length s) 1))))

; String -> 1String
; extracts the first character from a string
(define (first-char s)
  (string-ith s 0))

; String -> 1String
; extracts the last character from a string
(define (last-char s)
  (string-ith s (- (string-length s) 1)))

; Editor 1String -> Editor
; appends character to the right of ed's pre
(define (append-pre ed s)
  (if (image-width-fits ed)
      (make-editor (string-append (editor-pre ed) s) (editor-post ed))
      ed))

; Editor -> Boolean
; checks if the current image will fit the backgroud width
(define (image-width-fits ed)
  (>= WIDTH (image-width (text-with-coursor (editor-pre ed) (editor-post ed)))))

; Editor -> Editor
; runs the editor
(define (run ed)
  (big-bang ed
    [on-key edit]
    [to-draw render]))

(run (make-editor "Hello " "World"))