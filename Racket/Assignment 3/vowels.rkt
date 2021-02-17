;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname vowels) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 03, Problem 2
;; ***************************************************

;; Vowels 
(define vowel-a #\a)
(define vowel-e #\e)
(define vowel-i #\i)
(define vowel-o #\o)
(define vowel-u #\u)

;; (total-vowels listof-strings) consumes a list of strings, listof-strings,
;; and produces the total number of lower-case vowels in all of the strings.
;; The vowels are "a", "e", "i", "o", and "u" for this function.

;; total-vowels: (listof Str) -> Nat
(define (total-vowels listof-strings)
  (cond [(empty? listof-strings) 0]
        [else (check-vowels (string->list (first listof-strings)) listof-strings)]))

;; Tests
(check-expect (total-vowels (cons "aeiou" empty)) 5)
(check-expect (total-vowels (cons "tlp" empty)) 0)
(check-expect (total-vowels (cons "tlp" (cons "ywq" (cons "pop" empty)))) 1)
              

;; (check-vowels listof-chars listof-strings) consumes a list of
;; characters, listof-chars, and a list of strings, listof-strings.
;; It checks for vowels in each string and adds them so it ouputs
;; the total lower-case vowels in all the strings.

;; Examples
(check-expect (check-vowels
               (cons #\h (cons #\e (cons #\l (cons #\l (cons #\o empty)))))
               (cons "hello" (cons "bye" empty))) 3)

;; check-vowels: (listof Char) (listof Str) -> Nat
(define (check-vowels listof-chars listof-strings) 
  (cond [(empty? listof-chars) (total-vowels (rest listof-strings))]
        [(or (equal? (first listof-chars) vowel-a)
             (equal? (first listof-chars) vowel-e)
             (equal? (first listof-chars) vowel-i)
             (equal? (first listof-chars) vowel-o)
             (equal? (first listof-chars) vowel-u)) (+ 1 (check-vowels (rest listof-chars) listof-strings))]
        [else (+ 0 (check-vowels (rest listof-chars) listof-strings))]))




