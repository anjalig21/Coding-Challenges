;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 03, Problem 1a
;; ***************************************************

;; (replace-word change-word wanted-word listof-strings) consumes two strings,
;; wanted-word and change-word, and a list of strings, listof-strings. It then
;; produces a new list where all occurrences of the first string
;; have been replaced by the second string

;; Examples
(check-expect
(replace-word "exam" "assessment" (cons "exam"(cons "content"(cons "assignment" empty))))
(cons "assessment" (cons "content" (cons "assignment" empty))))
(check-expect
(replace-word "exam" "assessment" (cons "exam"(cons "exam"(cons "assignment" empty))))
(cons "assessment" (cons "assessment" (cons "assignment" empty))))
(check-expect
(replace-word "exam" "assessment" (cons "content"(cons "exam"(cons "assignment" empty))))
(cons "content" (cons "assessment" (cons "assignment" empty))))

;; replace-word: Str Str (listof Str) -> (listof Str)
(define (replace-word change-word wanted-word listof-strings)
  (cond [(empty? listof-strings) empty]
        [(cons? listof-strings) (insert change-word wanted-word listof-strings)]))

;; Tests
(check-expect (replace-word "hello" "bye" empty) empty)
(check-expect
(replace-word "exam" "assessment" (cons "exam"(cons "exam"(cons "exam" empty))))
(cons "assessment" (cons "assessment" (cons "assessment" empty))))
(check-expect
(replace-word "exam" "exam" (cons "exam"(cons "content"(cons "assignment" empty))))
(cons "exam" (cons "content" (cons "assignment" empty))))



;; (insert change-word wanted-word listof-strings) consumes two strings,
;; wanted-word and change-word, and a list of strings, listof-strings. It then
;; produces a new list where all occurrences of the first string
;; have been replaced by the second string. This function does all the recursion.

;; Examples
(check-expect
(replace-word "exam" "assessment" (cons "exam"(cons "exam"(cons "exam" empty))))
(cons "assessment" (cons "assessment" (cons "assessment" empty))))

;; insert: Str Str (listof Str) -> (listof Str)
(define (insert change-word wanted-word listof-strings) ;; are we allowed to use string=?
  (cond [(empty? listof-strings) empty]
        [(string=? change-word (first listof-strings))
         (cons wanted-word (insert change-word wanted-word (rest listof-strings)))]
        [else (cons (first listof-strings) (insert change-word wanted-word (rest listof-strings)))]))



;; ***************************************************
;; Problem 1b
;; ***************************************************

;; (add nat1 nat2) consumes two natural numbers,
;; nat1 and nat2, and produces their sum.

;; Examples
(check-expect (add 2 3) 5)
(check-expect (add 0 3) 3)

;; add: Nat Nat -> Nat
(define (add nat1 nat2)
  (cond [(= nat2 0) nat1]
        [else (add (add1 nat1) (sub1 nat2))]))

;; Tests
(check-expect (add 0 0) 0)
(check-expect (add 5 0) 5)
(check-expect (add 5 5) 10)


;; ***************************************************
;; Problem 1c
;; ***************************************************

;; (mult nat1 nat2) consumes two natural numbers,
;; nat1 and nat2, and produces their product.

;; Examples
(check-expect (mult 2 3) 6)
(check-expect (mult 4 4) 16)

;; mult: Nat Nat -> Nat
(define (mult nat1 nat2)
  (cond [(or (= nat1 0) (= nat2 0)) 0]
        [else (mult-recursion nat1 nat1 nat2)]))

;; Tests
(check-expect (mult 0 1) 0)
(check-expect (mult 21 1) 21)
(check-expect (mult 0 0) 0)
(check-expect (mult 1 0) 0)
(check-expect (mult 10 10) 100)


;; (mult-recursion nat1 nat1-constant nat2) consumes three natural numbers,
;; nat1, nat1-constant and nat2, and produces the product of nat1 and nat2.

;; Examples
(check-expect (mult-recursion 4 4 4) 16)
(check-expect (mult-recursion 3 3 4) 12)


;; Nat Nat Nat -> Nat
;;    requires nat>0
(define (mult-recursion nat1 nat1-constant nat2)
  (cond [(= (sub1 nat2) 0) nat1]
        [else (mult-recursion (add nat1 nat1-constant) nat1-constant (sub1 nat2))]))




            



