;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 02, Problem 4a
;; ***************************************************

;; (change-due costs paid) consumes the list of numbers, costs
;; and the non-negative number paid which represents the amount of money the
;; customer is providing as payment. It then produces the amount of change
;; the customer should receive back

;; Examples
(check-expect(change-due (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))) 100) 6.07)
(check-expect(change-due (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))) 93.93) 0)

;; change-due: (listof-Num) Num -> Num
;;    requires: (listof-Num) >= 0 and Num >= 0
(define (change-due costs paid)
  (- paid (sumof-costs costs)))

;; Tests
(check-expect(change-due (cons 0 empty) 93.93) 93.93)
(check-expect(change-due empty 93.93) 93.93)



;; (sumof-costs costs) consumes a list of numbers, costs,
;; and outputs the total cost

;; Examples
(check-expect(sumof-costs (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty))))) 93.93)

;; sum-ofcosts: (listof-Num) -> Num
;;    requires: (listof-Num) >= 0 
(define (sumof-costs costs)
  (cond [(empty? costs) 0]
        [else (+ (first costs) (sumof-costs (rest costs)))]))



;; ***************************************************
;; Problem 4b
;; ***************************************************

;; (paid-enough? costs paid) consumes the list of numbers costs
;; and the non-negative number paid and produces true if the
;; customer has paid enough to cover the costs and false otherwise

;;Examples
(check-expect(paid-enough? (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))) 100) true)
(check-expect (paid-enough? (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))) 50) false)

;; paid-enough?: (listof-Num) Num-> Bool
;;    requires: (listof-Num) >= 0 and Num >= 0
(define (paid-enough? costs paid)
  (cond [(>= paid (sumof-costs costs)) true]
        [else false]))

;; Tests
(check-expect (paid-enough? (cons 0 empty)50) true)
(check-expect (paid-enough? empty 50) true)
(check-expect (paid-enough? (cons 0 empty) 0) true)
(check-expect (paid-enough? (cons 5.00 (cons 6.00 (cons 4.99 empty)))
                            15.99) true)



;; ***************************************************
;; Problem 4c
;; ***************************************************

;; (free-item costs paid) consumes the list of numbers costs and the nonnegative number,
;; paid, and produces the first item in the list that is large enough that if
;; reduced to 0 would mean the customer has enough to pay for their bill.

;; Examples
(check-expect(free-item (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))) 80) 23.3)
(check-expect(free-item (cons 2.65 (cons 23.30 (cons 7.99 (cons 59.99 empty)))) 60) 59.99)

;; free-item: (ne-listof Num) Num -> Num
;;    requires: (ne-listof Num) >= 0 and Num >= 0
(define (free-item costs paid)
    (cond [(>= (first costs) (pending costs paid)) (first costs)]
          [else (free-item-recursion costs (rest costs) paid)]))

;; Tests
(check-expect(free-item (cons 59.99 (cons 23.30 (cons 7.99 (cons 2.65 empty)))) 60) 59.99)
(check-expect(free-item (cons 59.99 empty) 40) 59.99)
(check-expect(free-item (cons 20 (cons 59.99 empty)) 40) 59.99)
(check-expect(free-item (cons 0.05 (cons 0.01 empty)) 0.01) 0.05)



;; (free-item-recursion costs changing-list paid) consumes the original list of numbers, costs,
;; another list of numbers, changing-list, which the recursion is applied to and
;; the amount the customer pays, paid. It produces the first item in the list
;; that is large enough that if reduced to 0 would mean the customer has enough
;; to pay for their bill.

;; Examples
(check-expect(free-item-recursion (cons 59.99 (cons 23.30 (cons 7.99 (cons 2.65 empty))))
                         (cons 23.30 (cons 7.99 (cons 59.99 empty))) 60) 59.99)

;; free-item-recursion: (ne-listofNum) (ne-listof Num) Num -> Num
;;    requires: (ne-listof Num) >= 0 and Num >= 0
(define (free-item-recursion costs changing-list paid)
    (cond [(>= (first changing-list) (pending costs paid)) (first changing-list)]
          [else (free-item-recursion costs (rest changing-list) paid)]))



;; (pending costs paid) consumes a list of numbers, costs, and the
;; amount paid by the customer, paid, and output the pending cost to
;; be paid.

;; Examples
(check-expect(pending (cons 59.99 (cons 23.30 (cons 7.99 (cons 2.65 empty)))) 80) 13.93)

;; pending: (listof-Num) Num -> Num
;;    requires: (listof-Num) >= 0 and Num >= 0
(define (pending costs paid)
  (- (sumof-costs costs) paid))




