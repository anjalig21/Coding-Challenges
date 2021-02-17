;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname not-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 05, Problem 1a
;; ***************************************************

(define-struct ls (first rest))
;; A (lsof X) is one of:
;; * 'nothing
;; * (make-ls X (lsof X))


;; (ls-length lsof-Any) consumes list of any, lsof-Any,
;; and produces the number of values in it.

;; Examples
(check-expect (ls-length (make-ls "!" (make-ls 'huh (make-ls 42 'nothing)))) 3)
(check-expect (ls-length (make-ls #\a (make-ls 0 (make-ls 100000 'nothing)))) 3)

;; ls-length: (lsof Any) -> Nat
(define (ls-length lsof-Any)
  (cond [(not (ls? lsof-Any)) 0]
        [(ls? lsof-Any) (add1 (ls-length (ls-rest lsof-Any)))]))

;; Tests
(check-expect (ls-length (make-ls 0 (make-ls "a" 'nothing))) 2)
(check-expect (ls-length (make-ls -1 'nothing)) 1)
(check-expect (ls-length (make-ls (list 1 2 3) 'nothing)) 1)
(check-expect (ls-length 'nothing) 0)


;; ***************************************************
;; Problem 1b
;; ***************************************************

;; (ls-max lsof-Num) consumes a list of numbers, lsof-Num,
;; that contains at least one value, and produces the largest value.

;; Examples
(check-expect (ls-max (make-ls 5 (make-ls 9 (make-ls 7 'nothing)))) 9)
(check-expect (ls-max (make-ls -2 (make-ls -9 (make-ls -1 'nothing)))) -1)

;; ls-max: (lsof Num) -> Num
;;  requires: ls to be non-empty
(define (ls-max lsof-Num)
  (cond [(not (ls? (ls-rest lsof-Num))) (ls-first lsof-Num)]
        [else (max (ls-first lsof-Num) (ls-max (ls-rest lsof-Num)))]))

;; Tests
(check-expect (ls-max (make-ls -2 'nothing)) -2)
(check-expect (ls-max (make-ls 5.99999 (make-ls 6.9999 'nothing))) 6.9999)
(check-expect (ls-max (make-ls 5.0001 (make-ls 5 (make-ls -5 'nothing)))) 5.0001)