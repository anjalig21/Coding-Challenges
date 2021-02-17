;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super-foldr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 08, Problem 5a
;; ***************************************************

;; A nested list of X (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))

;; (super-foldr combine base nested-list) consumes a
;; function, combine, a base and a nested list. It then generalizes
;; foldr to  work on nested lists of non-list elements, folding each nested
;; list with the given function to produce the result to be used in
;; the parent list’s fold.

;; Examples
(check-expect (super-foldr + 0 '(1 (5 5 (1 3)) (10 2) 2)) 29)
(check-expect (super-foldr - 0 '(1 (5 5 (1 3)) (10 2) 2)) 9)
(check-expect (super-foldr (lambda (val sofar)
                             (cond [(string? val) (+ (string-length val) sofar)]
                                   [(number? val) (+ val sofar)])) 0 '("pancho" "lefty" ("my" "proud" "mountains")
                                                                                ("tecumseh" "valley"))) 41)

;; super-foldr: (X -> Y) Y (nested-listof X) -> Y
(define (super-foldr combine base nested-list)
  (cond [(empty? nested-list) base]
        [(list? (first nested-list)) (combine (combine (first (first nested-list))
                                                       (super-foldr combine base (rest (first nested-list))))
                                              (super-foldr combine base (rest nested-list)))]
        [else (combine (first nested-list)
                       (super-foldr combine base (rest nested-list)))]))

;; Tests
(check-expect (super-foldr + 0 empty) 0)


;; ***************************************************
;; Problem 5b
;; ***************************************************

;; (magnitudes nl) consumes a nested list and produces the sum of all
;; the absolute values of the numbers in a nested list.

;; Examples
(check-expect (magnitudes '(1 (-5 -5 (1 -3)) (-10 2) 2)) 29)
(check-expect (magnitudes '(1 (-1 -1 (1 -1)) (-1 -1) 1)) 8)

;; magnitudes: (nested-listof Num) -> Num
(define (magnitudes nl)
  (super-foldr (lambda (first-nl rror)
                 (+ (abs first-nl) rror)) 0 nl))

;; Tests
(check-expect (magnitudes empty) 0)
(check-expect (magnitudes '(0 -6 (-8))) 14)


;; ***************************************************
;; Problem 5c
;; ***************************************************

;; (super-filter pred nl) consumes a predicate, pred
;; and a nested list, nl and applies the filter function
;; on a nested list.

;; Examples
(check-expect (super-filter odd? '(1 (2 (2 3 4) 5 6 (7 8 9)) 10 11 12))
              '(1 ((3) 5 (7 9)) 11))
(check-expect (super-filter symbol? '(tray fare (1 tray 5) 9))
              '(tray fare (tray)))

;; super-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-filter pred nl)
  (super-foldr (lambda (first-nl rror)
                 (cond [(list? first-nl) (cons first-nl rror)]
                       [(pred first-nl) (cons first-nl rror)]
                       [else rror])) empty nl))

;; Tests
(check-expect (super-filter boolean? empty) empty)
(check-expect (super-filter even? '(1 (1 5 7) (7) (9) 101))
              (list empty empty empty))