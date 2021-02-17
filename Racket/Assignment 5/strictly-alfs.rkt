;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname strictly-alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 08, Problem 3a
;; ***************************************************

;; (occurrences num listof-nums) consumes a number, nums and a
;; list of numbers, listof-nums, and produces the number of times
;; that the given number occurs in the list of numbers.

;; Examples
(check-expect (occurrences 2 '(1 2 1 2 2 3 1)) 3)
(check-expect (occurrences 2 '(0 9 8 7 6 100)) 0)
(check-expect (occurrences 98 '(0 9 8 7 6 98)) 1)

;; occurrences: Num (listof Num) -> Nat
(define (occurrences num listof-nums)
  (foldr (lambda (first-num rror-num) (add1 rror-num)) 0 (filter (lambda (num1) (cond [(= num1 num) true]
                                                                                   [else false])) listof-nums)))

;; Tests
(check-expect (occurrences 20 empty) 0)
(check-expect (occurrences 20 '(0 9 8 20 6 98)) 1)
(check-expect (occurrences -20 '(-20 9 8 20 6 98)) 1)
(check-expect (occurrences 4 '(4 4 4 40 4 4)) 5)


;; ***************************************************
;; Problem 3b
;; ***************************************************

;; (zip lst1 lst2) consumes two lists of equal length, lst1 and lst2,
;; and produces a list of pairs (two element lists) where the ith pair
;; contains the ith element of the first list followed by the ith element
;; of the second list.

;; Examples
(check-expect (zip '(1 2 3) '(a b c)) '((1 a)(2 b)(3 c)))
(check-expect (zip '(-1 2 a) '(a b c)) '((-1 a)(2 b)(a c)))
(check-expect (zip empty empty) empty)

;; zip: (listof Any) (listof Any) -> (listof (list Any Any))
;;  requires ls1 and lst2 to be the same length
(define (zip lst1 lst2)
  (foldr (lambda (first-lst1 first-lst2 AL)
           (cons (list first-lst1 first-lst2) AL)) empty lst1 lst2))

;; Tests
(check-expect (zip '(-1 2 a &) '(a b c &)) '((-1 a)(2 b)(a c)(& &)))
(check-expect (zip '(1 1 1 1) '(1 1 1 1)) '((1 1)(1 1)(1 1)(1 1)))
(check-expect (zip '(1 0) '(0 1)) '((1 0)(0 1)))
(check-expect (zip '(1) '(0)) '((1 0)))


;; ***************************************************
;; Problem 3c
;; ***************************************************

;; (unzip pairs) consumes a list of pairs, pairs and produces a list
;; of two lists. The first list contains the first element from each
;; pair, and the second list contains the second element from each pair,
;; in the original order.

;; Examples
(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))
(check-expect (unzip '()) '(()()))
(check-expect (unzip '((1 1)(1 1)(1 1)(1 1))) '((1 1 1 1) (1 1 1 1)))

;; unzip: (listof (list Any Any)) -> (list (listof Any) (listof Any))
(define (unzip pairs)
  (list (foldr (lambda (pair rror)
           (cons (first pair) rror)) empty pairs)
        (foldr (lambda (pair rror)
           (cons (second pair) rror)) empty pairs)))

;; Tests
(check-expect (unzip '((1 0))) '((1) (0)))
(check-expect (unzip '((1 0)(0 1))) '((1 0)(0 1)))
(check-expect (unzip '((-1 a)(2 b)(a c))) '((-1 2 a)(a b c)))


;; ***************************************************
;; Problem 3d
;; ***************************************************

;; (subsequence lst from to) consumes a list, lst and two
;; natural numbers, from and to. It produces the subsequence
;; from lst that begins at index from and ends just before
;; index to. Indexing starts at 0.

;; Examples
(check-expect (subsequence '(a b c d e f g) 1 4) '(b c d))
(check-expect (subsequence '(a b c d e f g) 1 1) '())
(check-expect (subsequence '(a b c d) 0 400) '(a b c d))

;; subsequence: (listof Any) Nat Nat -> (listof Any)
(define (subsequence lst from to)
  (foldr (lambda (first-lst1 first-lst2 rror)
           (cond [(and (>= first-lst2 from) (< first-lst2 to)) (cons first-lst1 rror)]
                 [else rror])) empty lst (build-list (length lst) (lambda (x) x))))

;; Tests
(check-expect (subsequence '(a v t g f) 0 1) '(a))
(check-expect (subsequence empty 0 1) empty)
(check-expect (subsequence '(a v t) 0 3) '(a v t))