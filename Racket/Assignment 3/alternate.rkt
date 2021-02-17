;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname alternate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 03, Problem 4a
;; ***************************************************

;; alt-nat-template: Nat -> Any
(define (alt-nat-template n)
  (cond [(zero? n) ...]
        [(even? n) (... n ...
                        ... (alt-nat-template (/ n 2)))]
        [(odd? n) (... n ...
                        ... (alt-nat-template (/ (sub1 n) 2)))]))


;; ***************************************************
;; Problem 4b
;; ***************************************************

;; (powers-of-2 nat) consumes a natural number, nat,
;; and determines how many times it can be divided by 2
;; before either reaching 0 or an odd number.

;; Examples
(check-expect (powers-of-2 12) 2)
(check-expect (powers-of-2 0) 0)

;; powers-of-2: Nat -> Nat
(define (powers-of-2 nat)
  (cond [(zero? nat) 0]
        [(odd? nat) 0]
        [(even? nat) (+ 1 (powers-of-2 (/ nat 2)))]))

;; Tests
(check-expect (powers-of-2 14) 1)
(check-expect (powers-of-2 20) 2)
(check-expect (powers-of-2 64) 6)
(check-expect (powers-of-2 2) 1)
