;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname change) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 09, Problem 4
;; ***************************************************

;; A ChangeList is a (listof (list Nat (listof Nat)))

;; (fewest-coins nat listof-nat) consumes a Nat, nat, representing the change
;; to create, and a (listof Nat), listof-nat representing the coin denominations
;; that are available. The function produces a list of the fewest coins needed
;; to create that change. In the case of a tie, it produces any solution with
;; a minimal number of coins.

;; Examples
(check-expect (fewest-coins 45 '(1 5 10 25 100 200)) '(25 10 10))
(check-expect (fewest-coins 27 '(1 9 19)) '(9 9 9))
(check-expect (fewest-coins 0 '(1 5 10 25 100 200)) '())

;; fewest-coins: Nat (listof Nat) -> (listof Nat)
;;   requires (listof Nat) has a 1 unit coin, and
;;   is sorted in increasing order with no duplicates
(define (fewest-coins change denominations)
  (local [;; (list-combs change-now count) consumes two natural numbers
          ;; change-now and count and produces the combinations of 2 numbers
          ;; adding up to change-now.
          ;; list-combs: Nat Nat -> (listof (list Nat Nat))
          (define (list-combs change-now count)
            (cond [(> count (floor (/ change-now 2))) empty]
                  [else (cons (list (- change-now count) count)
                              (list-combs change-now (add1 count)))]))
          ;; (split-process split-change change-list-now) consumes a Nat,
          ;; split-change, and a ChangeList, change-list-now
          ;; and outputs the AL beside split-change.
          ;; split-process: Nat ChangeList -> (listof Nat)
          (define (split-process split-change change-list-now)
            (foldr (lambda (x rror)
                     (cond [(= (first x) split-change) (second x)]
                           [else rror])) empty change-list-now))
          ;; (smallest-change current-change splits smallest-count change-list) consumes
          ;; a Nat, current-change, a ChangeList, splits, a Nat, smallest-count and a
          ;; list of natural numbers, change-list. It then produces
          ;; the fewest change required.
          ;; smallest-change: Nat ChangeList Nat (listof Nat) -> (listof Nat)
          (define (smallest-change current-change splits smallest-count change-list)
            (local [(define first-split
                      (cond [(empty? splits) empty]
                            [else (split-process (first (first splits))
                                                 current-change)]))
                    (define second-split
                      (cond [(empty? splits) empty]
                            [else (split-process (second (first splits))
                                     current-change)]))
                    (define split1-length (length first-split))
                    (define split2-length (length second-split))]
              (cond [(empty? splits) change-list]
                  [else (cond [(< (+ split1-length split2-length) smallest-count)
                                 (smallest-change current-change (rest splits) (+ split1-length split2-length) (append first-split second-split))]
                                [else (smallest-change current-change (rest splits) smallest-count change-list)])])))
          ;; (make-change-list counter change-list-now) consumes a natural number, counter,
          ;; and a ChangeList, change-list-now and produces the ChangeList.
          ;; make-change-list: Nat ChangeList -> ChangeList
          (define (make-change-list counter change-list-now)
            (cond [(> counter change) change-list-now]
                  [(member? counter denominations)
                   (make-change-list (add1 counter) (cons (list counter (list counter)) change-list-now))]
              [else (make-change-list (add1 counter)
                                      (cons (list counter
                                                  (smallest-change change-list-now (rest (list-combs counter 0)) (add1 change) empty))
                                            change-list-now))]))]
    (cond [(= change 0) empty]
          [else (quicksort (second (first (make-change-list 2 '((1 (1)))))) >=)])))

;; Tests
(check-expect (fewest-coins 1 '(0 1)) '(1))
(check-expect (fewest-coins 24 '(0 1 4 5)) '(5 5 5 5 4))
(check-expect (fewest-coins 26 '(1 9 19)) '(19 1 1 1 1 1 1 1))
(check-expect (fewest-coins 105 '(1 4 5 15 78)) '(78 15 4 4 4))