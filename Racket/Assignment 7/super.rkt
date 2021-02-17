;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 07, Problem 1a
;; ***************************************************

;; A nested list of X (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))

;; (super-filter pred? lst) consumes a predicate, pred? and a nested list of X,
;; lst, and generalizes filter to work on nested lists,
;; applying the predicate to filter non-list elements in each nested list.

;; Examples
(check-expect (super-filter odd? (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list 1 (list (list 3) 5 (list 7 9)) 11))
(check-expect (super-filter even? (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list (list 2 (list 2 4) 6 (list 8)) 10 12))

;; super-filter: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-filter pred? lst)
  (cond [(empty? lst) empty]
        [(list? (first lst))
         (cons (super-filter pred? (first lst)) (super-filter pred? (rest lst)))]
        [(pred? (first lst))
         (cons (first lst) (super-filter pred? (rest lst)))]
        [else (super-filter pred? (rest lst))]))

;; Tests
(check-expect (super-filter zero? (list 0 8 7 5 6)) (list 0))
(check-expect (super-filter boolean? (list 9 (list true false) (list 0 9 true) false))
              (list (list true false) (list true) false))
(check-expect (super-filter symbol? (list 'a "a" "b" 'b (list 0 'hello)))
              (list 'a 'b (list 'hello)))

;; ***************************************************
;; Problem 1b
;; ***************************************************

;; (ruthless lst) consumes a nested list of X, lst and 
;; removes the symbol 'ruth from a nested list of symbols.

;; Examples
(check-expect (ruthless (list 'rabbit (list 'apple 'pluto (list 'ruth 'blue) 'ruth) 'hello))
              (list 'rabbit (list 'apple 'pluto (list 'blue)) 'hello))
(check-expect (ruthless (list 'rabbit (list 'apple 'pluto (list 'blue)) 'hello))
              (list 'rabbit (list 'apple 'pluto (list 'blue)) 'hello))

;; ruthless: (nested-listof Sym) -> (nested-listof Sym)
(define (ruthless lst)
  (local [;; (not-ruth? symbol) consumes a symbol, sym, and produces true
          ;; if it is not the symbol 'ruth and false otherwise.
          ;; not-ruth?: Sym -> Bool
          (define (not-ruth? symbol) (not (symbol=? symbol 'ruth)))]
    (super-filter not-ruth? lst)))

;; Tests
(check-expect (ruthless empty) empty)
(check-expect (ruthless (list 'ruth (list 'ruth 'ruth) 'ruth)) (list empty))
(check-expect (ruthless (list 'ruth)) empty)


;; ***************************************************
;; Problem 1c
;; ***************************************************

;; (supersize n lst) consumes a number, n and a nested list of X, lst,
;; and removes all numbers less than n from a nested list of natural numbers.

;; Examples
(check-expect (supersize 4 (list 8 1 (list 2 6 3) 10 1))
              (list 8 (list 6) 10))
(check-expect (supersize 0 (list 8 1 (list 2 6 3) 10 1))
              (list 8 1 (list 2 6 3) 10 1))

;; supersize: Num (nested-listof Nat) -> (nested-listof Nat) 
(define (supersize n lst)
  (local [;; (less-than-n? num) consumes a number, num, and produces
          ;; false if the number is less than n and true otherwise.
          ;; less-than-n?: Num -> Bool
          (define (less-than-n? num) (not (< num n)))]
    (super-filter less-than-n? lst)))

;; Tests
(check-expect (supersize 100 (list 8 1 (list 2 6 3) 10 1))
              (list empty))
(check-expect (supersize 0 (list -8 1 (list 2 6 3) 10 1))
              (list 1 (list 2 6 3) 10 1))
(check-expect (supersize -5 (list -6)) empty)

;; ***************************************************
;; Problem 1d
;; ***************************************************

;; (super-keeper pred? lst), consumes a predicate, pred? and a nested
;; list of X, list and produces a list with the elements of lst for
;; which the predicate pred? produces a false value.

;; Examples
(check-expect (super-keeper odd? (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list (list 2 (list 2 4) 6 (list 8)) 10 12))
(check-expect (super-keeper even? (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
              (list 1 (list (list 3) 5 (list 7 9)) 11))

;; super-keeper: (X -> Bool) (nested-listof X) -> (nested-listof X)
(define (super-keeper pred? lst)
  (local [;; (not-pred? x) consumes any value, x,
          ;; and outputs the opposite of what the original
          ;; predict would produce.
          ;; not-pred?: X -> Bool
          (define (not-pred? X) (not (pred? X)))]
    (super-filter not-pred? lst)))

;; Tests
(check-expect (super-keeper zero? (list 0 8 7 5 6)) (list 8 7 5 6))
(check-expect (super-keeper boolean? (list 9 (list true false) (list 0 9 true) false))
              (list 9 empty (list 0 9)))
(check-expect (super-keeper symbol? (list 'a "a" "b" 'b (list 0 'hello)))
              (list "a" "b" (list 0)))