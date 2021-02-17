;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname helper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct silly-string (first middle last))



(check-expect (unsillify (make-silly-string #\L (make-silly-string #\o (make-silly-string #\v (make-silly-string #\e empty #\l) #\a) #\c) #\e))
              "Lovelace")
(check-expect (unsillify (make-silly-string #\B (make-silly-string #\a (make-silly-string #\b #\b #\a) #\g) #\e))
              "Babbage")
              
(define (unsillify ss)
  (string-append (list->string (unsillify-first-half ss)) (list->string (unsillify-last-half ss))))

(define (unsillify-first-half ss)
  (cond [(empty? ss) empty]
        [(char? ss) (cons ss empty)]
        [else (cons (silly-string-first ss) (unsillify-first-half (silly-string-middle ss)))]))

(define (unsillify-last-half ss)
  (cond [(empty? ss) empty]
        [(char? ss) empty]
        [else (append (unsillify-last-half (silly-string-middle ss)) (list (silly-string-last ss)))]))



;; (define (yani-reverse L) [if (null? L) null [append (yani-reverse (rest L))(list [first L])]])