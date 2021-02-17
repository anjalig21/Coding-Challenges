;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname factors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 03, Problem 6a
;; ***************************************************

;; Unusual Numbers
(define unusualnum-0 0)
(define unusualnum-1 1)

;; (all-factors n) consumes a natural number n, and produces a list
;; of all natural numbers x where 0 < x < n and x divides n evenly.

;; Examples
(check-expect (all-factors 30)
              (cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15 empty))))))))
(check-expect (all-factors 0) empty)
(check-expect (all-factors 1) empty)

;; all-factors: Nat -> (listof Nat)
(define (all-factors n)
  (cond [(or (= n unusualnum-0) (= n unusualnum-1)) empty]
        [else (check-factors n 1)]))

;; Tests
(check-expect (all-factors 2) (cons 1 empty))
(check-expect (all-factors 3) (cons 1 empty))
(check-expect (all-factors 17) (cons 1 empty))
(check-expect (all-factors 32)
               (cons 1 (cons 2 (cons 4 (cons 8 (cons 16 empty))))))


;; (check-factors n all-nums) consumes two natural numbers, n and all-nums,
;; and produces a list of all natural numbers x where 0 < x < n and x divides
;; n evenly. This function does all the recursion.

;; Examples
(check-expect (check-factors 30 1)
              (cons 1 (cons 2 (cons 3 (cons 5 (cons 6 (cons 10 (cons 15 empty))))))))

;; check-factors: Nat Nat -> (listof Nat)
(define (check-factors n all-nums)
  (cond [(>= all-nums n) empty]
        [(false? (= (remainder n all-nums) 0)) (check-factors n (add1 all-nums))]
        [else (cons all-nums (check-factors n (add1 all-nums)))]))



;; ***************************************************
;; Problem 6b
;; ***************************************************

;; (is-prime? nat) consumes a natural number, nat,
;; and produces true if it is prime, and false otherwise.

;; Examples
(check-expect (is-prime? 30) false)
(check-expect (is-prime? 7) true)
(check-expect (is-prime? 1) false)

;; is-prime?: Nat -> Bool
(define (is-prime? nat)
  (cond [(or (= nat unusualnum-0) (= nat unusualnum-1)) false]
        [(= (length (all-factors nat)) 1) true]
        [else false]))

;; Tests
(check-expect (is-prime? 25) false)
(check-expect (is-prime? 29) true)
(check-expect (is-prime? 81) false)
(check-expect (is-prime? 0) false)


;; ***************************************************
;; Problem 6c
;; ***************************************************

;; (is-composite? nat) consumes a natural number, nat,
;; and produces true if it is a composite number, and false otherwise.

;; Examples
(check-expect (is-composite? 30) true)
(check-expect (is-composite? 7) false)
(check-expect (is-composite? 0) false)

;; is-composite?: Nat -> Bool
(define (is-composite? nat)
  (cond [(or (= nat unusualnum-0) (= nat unusualnum-1)) false]
        [(false? (is-prime? nat)) true]
        [else false]))

;; Tests
(check-expect (is-composite? 3) false)
(check-expect (is-composite? 21) true)
(check-expect (is-composite? 100) true)
(check-expect (is-composite? 1) false)



