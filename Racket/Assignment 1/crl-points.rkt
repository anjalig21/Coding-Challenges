;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname crl-points) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 01, Problem 4
;; ***************************************************


;; Points
(define first-place 50)
(define second-place 20)
(define third-place 10)


;; (crl-points a b c d) consumes 3 numbers: a, for first place wins
;; b, for second place wins, c for third place wins and a symbol, d
;; outlining the violations, and outputs total points.


;; Examples
(check-expect(crl-points 0 2 4 'minor-violation) 90)
(check-expect(crl-points 1 0 0 'good-standing) 50)
(check-expect(crl-points 1 2 0 'major-violation) 67)


;; crl-points: Nat Nat Nat Sym -> Nat
;;    requires Sym = 'major-violation, 'minor-violation, 'disqualified or 'good-standing


(define (crl-points a b c d)
  (floor(*(+ (* a first-place) (* b second-place) (* c third-place) (wins a b c)) (violations d))))


;; Tests
(check-expect(crl-points 0 0 0 'major-violation) 0)
(check-expect(crl-points 0 1 0 'minor-violation) 19)
(check-expect(crl-points 8 0 4 'disqualified) 0)
(check-expect(crl-points 1 4 1 'good-standing) 155)



;; (wins a b c) consumes 3 numbers: a, for first place wins
;; b, for second place wins, c for third place wins, and
;; checks if total wins are 5 or above. If they are, it
;; outputs 15 which is the bonus points.


;; Examples
(check-expect(wins 0 2 3) 15)
(check-expect(wins 0 0 3) 0)


;; wins: Nat Nat Nat -> Nat


(define (wins a b c)
  (cond [(>= (+ a b c) 5) 15]
        [else 0]))



;; (violations d) consumes a symbol d and evaluates the violation deduction


;; Examples
(check-expect(violations 'minor-violation) 0.95)


;; violations: Sym -> Num


(define (violations d)
  (cond [(equal? d 'minor-violation) 0.95]
        [(equal? d 'major-violation) 0.75]
        [(equal? d 'disqualified) 0]
        [(equal? d 'good-standing) 1]))