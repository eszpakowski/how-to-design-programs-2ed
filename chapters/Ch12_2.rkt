;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ch12_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

(define date1 (create-date 2010 5 31 18 52 27))
(define date2 (create-date 2011 2 27 21 45 44))

(define track1 (create-track "Smells Like Teen Spirit" "Nirvana" "Nevermind" 301165 1 (create-date 2011 2 27 21 45 46) 2 (create-date 2011 2 27 21 58 34)))
(define track2 (create-track "A No-Rough-Stuff Type Deal" "Breaking Bad" "Breaking Bad, Season 1" 2855522 7 (create-date 2010 5 31 18 4 26) 1 (create-date 2010 5 31 18 52 27)))
(define track3 (create-track "Machinehead" "Bush" "Sixteen Stone" 256417 7 (create-date 2011 2 27 21 45 47) 1 (create-date 2011 2 27 21 51 49)))
(define track4 (create-track "Machinehead2" "Bush" "Sixteen Stone" 256417 7 (create-date 2011 2 27 21 45 47) 1 (create-date 2000 2 27 21 51 49)))

(define list-of-tracks1 (list track1 track2 track3))

; LTracks -> N
; sums total amount of play time for a list of tracks
(check-expect (total-time '()) 0)
(check-expect (total-time (list track1)) 602330)
(check-expect (total-time list-of-tracks1) 3714269)

(define (total-time l)
  (cond
    [(empty? l) 0]
    [else
     (+ (sum-total-time (first l))
        (total-time (rest l)))]))

; Track -> N
(define (sum-total-time t)
  (* (track-play# t) (track-time t)))

; LTracks -> List-of-strings
; returns all album titles for a list of tracks
(check-expect (select-all-album-titles '()) '())
(check-expect (select-all-album-titles (list track1))
              (list "Nevermind"))
(check-expect (select-all-album-titles list-of-tracks1)
              (list "Nevermind" "Breaking Bad, Season 1" "Sixteen Stone"))

(define (select-all-album-titles l)
  (cond
    [(empty? l) '()]
    [else
     (cons (track-album (first l))
           (select-all-album-titles (rest l)))]))

; List-of-strings -> List-of-strings
(check-expect (create-set '()) '())
(check-expect (create-set (list "Nevermind"))
              (list "Nevermind"))
(check-expect (create-set (list "Nevermind" "Nevermind" "Nevermind"))
              (list "Nevermind"))
(check-expect (create-set (list "Nevermind" "Breaking Bad, Season 1" "Sixteen Stone"))
              (list "Nevermind" "Breaking Bad, Season 1" "Sixteen Stone"))
(check-expect (create-set (list "Nevermind" "Nevermind"
                                "Breaking Bad, Season 1" "Breaking Bad, Season 1" "Breaking Bad, Season 1"
                                "Sixteen Stone" "Sixteen Stone" "Sixteen Stone" "Sixteen Stone"))
              (list "Nevermind" "Breaking Bad, Season 1" "Sixteen Stone"))

(define (create-set l)
  (cond
    [(empty? l) '()]
    [else
     (add-if-not-present (first l)
                         (create-set (rest l)))]))

; String List-of-strings -> List-of-strings
(define (add-if-not-present s l)
  (cond
    [(empty? l) (cons s l)]
    [else
     (if (member? s l)
         l
         (cons s l))]))

; LTracks -> List-of-strings
; produces a list of unique album titles from a list of tracks
(check-expect (select-album-titles/unique '()) '())
(check-expect (select-album-titles/unique (list track1))
              (list "Nevermind"))
(check-expect (select-album-titles/unique list-of-tracks1)
              (list "Nevermind" "Breaking Bad, Season 1" "Sixteen Stone"))
(check-expect (select-album-titles/unique (list track1 track1 track1 track1
                                                track2 track2 track2 track2 track2
                                                track3 track3 track3 track3 track3 track3))
              (list "Nevermind" "Breaking Bad, Season 1" "Sixteen Stone"))

(define (select-album-titles/unique l)
  (create-set (select-all-album-titles l)))

; String LTracks -> LTracks
; extracts tracks from the given album
(check-expect (select-album "Nevermind" '()) '())
(check-expect (select-album "Nevermind" (list track1))
              (list track1))
(check-expect (select-album "Nevermind" list-of-tracks1)
              (list track1))
(check-expect (select-album "Sixteen Stone" (list track1 track1 track1 track1
                                                  track2 track2 track2 track2 track2
                                                  track3 track3 track3 track3 track3 track3))
              (list track3 track3 track3 track3 track3 track3))


(define (select-album s l)
  (cond
    [(empty? l) '()]
    [else (add-if-matching s (first l)
                           (select-album s (rest l)))]))

; String Track -> LTracks
(define (add-if-matching s t l)
  (if (string=? (track-album t) s)
      (cons t l)
      l))

; String Date LTracks -> LTracks
; extracts list of tracks that belong to the given album
; and have been played after the given date
(check-expect (select-album-date "Nevermind" (create-date 2010 5 31 18 52 27) '()) '())
(check-expect (select-album-date "Nevermind" (create-date 2010 5 31 18 52 27) (list track1))
              (list track1))
(check-expect (select-album-date "Nevermind" (create-date 2010 5 31 18 52 27) list-of-tracks1)
              (list track1))
(check-expect (select-album-date "Sixteen Stone" (create-date 2010 5 31 18 52 27) (list track1 track2 track3 track4))
              (list track3))

(define (select-album-date album after l)
  (cond
    [(empty? l) '()]
    [else (album-matches-and-after album after (first l)
                                   (select-album-date album after (rest l)))]))

; String Date Track LTracks -> LTracks
(define (album-matches-and-after album date t l)
  (if (and (string=? (track-album t) album)
           (after? (track-played t) date))
      (cons t l)
      l))

; Date Date -> Boolean
(check-expect (after? (create-date 2000 1 10 10 10 10) (create-date 2000 1 10 10 10 10))
              #false)
(check-expect (after? (create-date 2001 1 10 10 10 10) (create-date 2000 1 10 10 10 10))
              #true)
(check-expect (after? (create-date 1999 1 10 10 10 10) (create-date 2000 1 10 10 10 10))
              #false)
(check-expect (after? (create-date 2000 2 10 10 10 10) (create-date 2000 1 10 10 10 10))
              #true)
(check-expect (after? (create-date 2000 0 10 10 10 10) (create-date 2000 1 10 10 10 10))
              #false)
(check-expect (after? (create-date 2000 1 11 10 10 10) (create-date 2000 1 10 10 10 10))
              #true)
(check-expect (after? (create-date 2000 1 9 10 10 10) (create-date 2000 1 10 10 10 10))
              #false)
(check-expect (after? (create-date 2000 1 10 11 10 10) (create-date 2000 1 10 10 10 10))
              #true)
(check-expect (after? (create-date 2000 1 10 9 10 10) (create-date 2000 1 10 10 10 10))
              #false)
(check-expect (after? (create-date 2000 1 10 10 11 10) (create-date 2000 1 10 10 10 10))
              #true)
(check-expect (after? (create-date 2000 1 10 10 9 10) (create-date 2000 1 10 10 10 10))
              #false)
(check-expect (after? (create-date 2000 1 10 10 10 11) (create-date 2000 1 10 10 10 10))
              #true)
(check-expect (after? (create-date 2000 1 10 10 10 9) (create-date 2000 1 10 10 10 10))
              #false)

(define (after? d1 d2)
  (or (> (date-year d1) (date-year d2))
      (and
       (= (date-year d1) (date-year d2))
       (> (date-month d1) (date-month d2)))
      (and
       (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (> (date-day d1) (date-day d2)))
      (and
       (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (> (date-day d1) (date-day d2)))
      (and
       (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (= (date-day d1) (date-day d2))
       (> (date-hour d1) (date-hour d2)))
      (and
       (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (= (date-hour d1) (date-hour d2))
       (> (date-minute d1) (date-minute d2)))
      (and
       (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (= (date-hour d1) (date-hour d2))
       (= (date-minute d1) (date-minute d2))
       (> (date-second d1) (date-second d2)))))

; List-of-LTracks is one of:
; - '()
; - (cons LTracks List-of-LTracks)

; LTracks -> List-of-LTracks
; creates a list of LTracks, one per album
; gets rid of duplicate based on track title
(check-expect (select-albums '()) '())
(check-expect (select-albums (list track1))
              (list (list track1)))
(check-expect (select-albums list-of-tracks1)
              (list (list track1) (list track2) (list track3)))
(check-expect (select-albums (list track1 track1 track1 track1
                                   track2 track2 track2 track2 track2
                                   track3 track3 track3 track3 track3 track3
                                   track4 track4 track4 track4 track4 track4 track4))
              (list (list track1) (list track2) (list track3 track4)))

(define (select-albums l)
  (group-by-album (select-album-titles/unique l) (create-set l)))


; List-of-strings LTracks -> List-of-LTracks
(define (group-by-album albums l)
  (cond
    [(empty? albums) '()]
    [else (cons (create-list (first albums) l)
                (group-by-album (rest albums) l))]))

; String LTracks -> LTracks
(define (create-list album l)
  (cond
    [(empty? l) '()]
    [else (add-if-matching-album (first l) album
                                 (create-list album (rest l)))]))

; String Track -> LTracks
(define (add-if-matching-album t album l)
  (if (string=? (track-album t) album)
      (cons t l)
      l))







