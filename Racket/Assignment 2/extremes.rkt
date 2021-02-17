;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname extremes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 02, Problem 2a
;; ***************************************************


;; (smallest nums) consumes a list of numbers, nums, and
;; outputs the smallest number

;; Examples
(check-expect(smallest (cons -5 empty)) -5)
(check-expect(smallest (cons -5 (cons 2 (cons 10.5 empty)))) -5)

;; smallest: (ne-listof Num) -> Num
(define (smallest nums)
  (cond [(empty? (rest nums)) (first nums)]
        [else (min (first nums)
                  (smallest (rest nums)))]))

;; Tests
(check-expect(smallest (cons 5 (cons 0 (cons 10.5 empty)))) 0)
(check-expect (smallest (cons 0(cons -5(cons 0 (cons 05 empty))))) -5)



;; ***************************************************
;; Problem 2b
;; ***************************************************

;; (largest nums) consumes a list of numbers, nums, and
;; outputs the largest number

;; Examples
(check-expect(largest (cons -5 empty)) -5)
(check-expect(largest (cons -5 (cons 2 (cons 10.5 empty)))) 10.5)

;; largest: (ne-listof Num) -> Num
(define (largest nums)
  (cond [(empty? (rest nums)) (first nums)]
        [else (max (first nums)
                  (largest (rest nums)))]))

;; Tests
(check-expect(largest (cons 0 (cons 5 (cons -2 empty)))) 5)
(check-expect (largest (cons 0 empty)) 0)
(check-expect (largest (cons -0.09 (cons -0.09 (cons -5 empty)))) -0.09)



;; ***************************************************
;; Problem 2c
;; ***************************************************

;; (max-diff nums) consumes a list of numbers, nums, and
;; outputs the largest difference between any two numbers

;; Examples
(check-expect(max-diff (cons -5 empty)) 0)
(check-expect(max-diff (cons -5 (cons 2 (cons 10.5 empty)))) 15.5)

;; max-diff: (ne-listof Num) -> Num
(define (max-diff nums)
  (cond [(empty? (rest nums)) 0]
        [else (- (largest nums) (smallest nums))]))

;; Tests
(check-expect(max-diff (cons 0 (cons 5 (cons -2 empty)))) 7)
(check-expect (max-diff (cons -10 (cons -19 (cons 20 empty)))) 39)
(check-expect (max-diff (cons 0 empty)) 0)
(check-expect (max-diff (cons 54 (cons 54 (cons -133 empty)))) 187)



