;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matches) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 08, Problem 4
;; ***************************************************

;; (matches-func? f pairs) consumes a function, f, and a list of
;; pairs, pairs. It then produces true if each pair in the list
;; is of the form (x (f x)), which is the second value in the pair
;; is the result of applying the provided function to the first value
;; in the pair, and false otherwise.

;; Examples
(check-expect (matches-func? sqr '((5 25) (2 4) (10 100))) true)
(check-expect (matches-func? add1 '((1 2) (3 5) (10 15))) false)
(check-expect (matches-func? add1 '((1 2) (3 4) (10 11))) true)

;; matches-func?: (X -> Y) (listof (list X Any)) -> Bool
(define (matches-func? f pairs)
  (foldr (lambda (first-x rror-x)
           (and first-x rror-x)) true (foldr (lambda (pair rror)
                                       (cond [(equal? (f (first pair)) (second pair)) (cons true rror)]
                                             [else (cons false rror)])) empty pairs)))

;; Tests
(check-expect (matches-func? sub1 '((1 0) (3 2) (10 9))) true)
(check-expect (matches-func? sqrt '((1 1) (9 b) (16 4))) false)
(check-expect (matches-func? sqrt empty) true)