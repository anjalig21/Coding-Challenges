;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname translations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 01, Problem 2a
;; ***************************************************


;; (volume r) computes the volume of a sphere with radius r.


;; Examples
(check-within (volume 2) 33.51 0.01)
(check-within (volume 5) 523.60 0.01)


;; volume: Num -> Num
;;    requires Num >= 0


(define (volume r) (* (/ 4 3) pi (expt r 3)))


;; Tests
(check-within (volume 4.2) 310.34 0.01)
(check-within (volume 0.01) 0.00000419 0.01)
(check-within (volume 0) 0 0.01)



;; ***************************************************
;; Problem 2b
;; ***************************************************


;; Golden ratio phi
(define phi (/ (+ 1 (sqrt 5)) 2))


;; (fib n) computes the fibonacci number at position n.


;; Examples
(check-within (fib 2) 1 0.01)
(check-within (fib 5) 5 0.01)


;; fib: Nat -> Nat
;;    requires n > 0


(define (fib n) (/ (- (expt phi n) (expt (* -1 phi) (* -1 n)))
                   (- (* 2 phi) 1)))


;; Tests
(check-within (fib 40) 102334155 0.01)
(check-within (fib 1) 1 0.01)
(check-within (fib 10) 55 0.01)
