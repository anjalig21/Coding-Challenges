;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 03, Problem 3
;; ***************************************************

;; (differences listof-nums) consumes a non-empty list of numbers,
;; listof-nums, and produces a list of the differences between
;; each number and the number before it.

;; Examples
(check-expect (differences (cons 4 (cons 7 (cons 1 empty)))) (cons 3 (cons -6 empty)))
(check-expect (differences (cons 1 empty)) empty)

;; differences: (ne-listof Num) -> (listof Num)
(define (differences listof-nums)
  (cond [(empty? (rest listof-nums)) empty]
        [(cons? listof-nums)
         (cons (- (first (rest listof-nums)) (first listof-nums))
               (differences (rest listof-nums)))]))

;; Tests
(check-expect (differences (cons -2 (cons 0 (cons -5 empty)))) (cons 2 (cons -5 empty)))
(check-expect (differences (cons -2.5 (cons 0.5 (cons -5.5 empty)))) (cons 3 (cons -6 empty)))
(check-expect (differences (cons -2 (cons -5 empty))) (cons -3 empty))

