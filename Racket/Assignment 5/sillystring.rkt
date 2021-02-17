;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sillystring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 05, Problem 3a
;; ***************************************************

(define-struct silly-string (first middle last))
;; A SillyStringStruct is a (make-silly-string Char SillyStr Char)
;; A SillyStr is one of:
;; * empty
;; * a Char
;; * a SillyStringStruct

;; ((sillify s) consumes a Str, s, and produces
;; the corresponding SillyStr.

;; Examples
(check-expect (sillify "Babbage")
              (make-silly-string #\B (make-silly-string #\a (make-silly-string #\b #\b #\a) #\g) #\e))
(check-expect (sillify "Lovelace")
              (make-silly-string #\L (make-silly-string #\o (make-silly-string #\v (make-silly-string #\e empty #\l) #\a) #\c) #\e))

;; sillify: Str -> SillyStr
(define (sillify s)
  (cond [(empty? (string->list s)) empty]
        [(= (length (string->list s)) 1) (first (string->list s))]
        [else (make-silly-string (first (string->list s))
                                 (sillify (list->string (middle-string (string->list s) (string->list s))))
                                 (last-char (string->list s)))]))

;; Tests
(check-expect (sillify "hello")
              (make-silly-string #\h (make-silly-string #\e #\l #\l) #\o))
(check-expect (sillify "hi")
              (make-silly-string #\h '() #\i))
(check-expect (sillify "bye")
              (make-silly-string #\b #\y #\e))
(check-expect (sillify "") empty)


;; (last-char listof-char) consumes a list of characters,
;; listof-char, and outputs the last character

;; Examples
(check-expect (last-char (list #\h #\e #\l #\l #\o)) #\o)
(check-expect (last-char (list #\h #\e)) #\e)

;; last-char: (ne-listof Char) -> Char
(define (last-char listof-char)
  (cond [(empty? (rest listof-char)) (first listof-char)]
        [else (last-char (rest listof-char))]))


;; (middle-string s) consumes a string, s,
;; and outputs the middle string.

;; Examples
(check-expect (middle-string (string->list "hello") (string->list "hello")) (list #\e #\l #\l))
(check-expect (middle-string (string->list "bye") (string->list "bye")) (list #\y))
(check-expect (middle-string (string->list "be") (string->list "be")) empty)
(check-expect (middle-string (string->list "w") (string->list "w")) (list #\w))

;; middle-string: Str -> Str
(define (middle-string s s-constant)
 (cond [(= (length s-constant) 1) s-constant]
       [(= (length s-constant) 2) empty]
       [(empty? (rest (rest s))) empty]
       [else (cons (second s) (middle-string (rest s) s-constant))]))

   
;; ***************************************************
;; Problem 3b
;; ***************************************************

;; (unsillify ss) does the opposite of sillify. It
;; consumes a SillyStr, ss, and produces the corresponding Str.

;; Examples
(check-expect (unsillify (make-silly-string #\L (make-silly-string #\o (make-silly-string #\v (make-silly-string #\e empty #\l) #\a) #\c) #\e))
              "Lovelace")
(check-expect (unsillify (make-silly-string #\B (make-silly-string #\a (make-silly-string #\b #\b #\a) #\g) #\e))
              "Babbage")

;; unsillify: SillyStr -> Str
(define (unsillify ss)
  (string-append (list->string (unsillify-first-half ss)) (list->string (unsillify-last-half ss))))

;; Tests
(check-expect (unsillify (make-silly-string #\h (make-silly-string #\e #\l #\l) #\o)) "hello")
(check-expect (unsillify (make-silly-string #\h '() #\i)) "hi")
(check-expect (unsillify (make-silly-string #\b #\y #\e)) "bye")
(check-expect (unsillify empty) "")


;; (unsillify-first-half ss) consumes a SillyStr, ss,
;; and produces the corresponding first-half of the string.

;; Examples
(check-expect (unsillify-first-half (make-silly-string #\L (make-silly-string #\o (make-silly-string #\v (make-silly-string #\e empty #\l) #\a) #\c) #\e))
              (list #\L #\o #\v #\e))
(check-expect (unsillify-first-half (make-silly-string #\b #\y #\e))
              (list #\b #\y))                                    

;; unsillify-first-half: SillyStr -> Str
(define (unsillify-first-half ss)
  (cond [(empty? ss) empty]
        [(char? ss) (cons ss empty)]
        [else (cons (silly-string-first ss) (unsillify-first-half (silly-string-middle ss)))]))


;; (unsillify-last-half ss) consumes a SillyStr, ss,
;; and produces the corresponding last-half of the string.

;; Examples
(check-expect (unsillify-last-half (make-silly-string #\L (make-silly-string #\o (make-silly-string #\v (make-silly-string #\e empty #\l) #\a) #\c) #\e))
              (list #\l #\a #\c #\e))
(check-expect (unsillify-last-half (make-silly-string #\b #\y #\e))
              (list #\e))

;; unsillify-last-half: SillyStr -> Str
(define (unsillify-last-half ss)
  (cond [(empty? ss) empty]
        [(char? ss) empty]
        [else (append (unsillify-last-half (silly-string-middle ss)) (list (silly-string-last ss)))]))
  

;; ***************************************************
;; Problem 3c
;; ***************************************************

;; (palindrome? ss) consumes a SillyStr, ss, and then produces
;; true if ss a palindrome, and false otherwise.

;; Examples
(check-expect (palindrome? (make-silly-string #\r (make-silly-string #\a #\d #\a) #\r)) true)
(check-expect (palindrome? (make-silly-string #\r (make-silly-string #\a empty #\a) #\r)) true)

;; palindrome?: SillyStr -> Bool
(define (palindrome? ss)
  (cond [(empty? ss) true]
        [(char? ss) true]
        [(and (char=? (silly-string-first ss) (silly-string-last ss)) (palindrome? (silly-string-middle ss))) true]
        [else false]))

;; Tests
(check-expect (palindrome? (make-silly-string #\s (make-silly-string #\o #\n #\a) #\r)) false)
(check-expect (palindrome? (sillify "racecar")) true)
(check-expect (palindrome? (sillify "Koenigsegg")) false)
(check-expect (palindrome? (sillify "")) true)