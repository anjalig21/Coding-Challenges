;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname division) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 03, Problem 5
;; ***************************************************

;; (divide a b) consumes two natural numbers, a and b, and produces
;; a list of exactly two numbers. The first element of the list is
;; the quotient when a is divided by b, and the second element is
;; the remainder.

;; Examples
(check-expect (divide 17 5) (cons 3 (cons 2 empty)))
(check-expect (divide 0 5) (cons 0 (cons 0 empty)))

;; divide: Nat Nat -> (listof Nat)
;;   requires b > 0
(define (divide a b)
  (cond [(zero? a) (cons 0 (cons 0 empty))]
        [else (cons (quotient-helper b b a) (cons (remainder-helper a b) empty))]))

;; Tests
(check-expect (divide 4 2) (cons 2 (cons 0 empty)))
(check-expect (divide 1 1) (cons 1 (cons 0 empty)))
(check-expect (divide 18 5) (cons 3 (cons 3 empty)))


;; (quotient-helper b b-constant a) consumes three natural numbers
;; b, b-constant and a, and produces the quotient of a/b.

;; Examples
(check-expect (quotient-helper 5 5 17) 3)

;; quotient-helper: Nat Nat Nat -> Nat
(define (quotient-helper b b-constant a)
  (cond [(< a b) 0]
        [else (+ 1 (quotient-helper (+ b b-constant) b-constant a))]))


;; (remainder-helper a b) consumes two natural numbers,
;; a and b, and produces the remainder of a/b.

;; Examples
(check-expect (remainder-helper 17 5) 2)

;; remainder-helper: Nat Nat -> Nat
(define (remainder-helper a b)
  (- a (* (quotient-helper b b a) b)))
