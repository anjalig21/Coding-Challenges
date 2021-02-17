;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 01, Problem 3
;; ***************************************************


;; (sequence-type a b c d) consumes 4 numbers a, b, c, d and states whether the
;; sequence is arithmetic, geometric, both or neither.


;; Examples
(check-expect(sequence-type 1 2 3 4) 'arithmetic)
(check-expect(sequence-type 1 2 4 8) 'geometric)


;; sequence-type: Num Num Num Num -> Sym


(define (sequence-type a b c d)
  (cond [(= a b c d) 'both]
        [(equal?(geo-detection a b c d) true) 'geometric]
        [(= (- b a) (- c b) (- d c)) 'arithmetic]
        [(equal?(zero-detection a b c d) true) 'neither]
        [(= (/ b a) (/ c b) (/ d c)) 'geometric]
        [else 'neither]))


;; Tests
(check-expect(sequence-type 0 0 0 0) 'both)
(check-expect(sequence-type 0 2 4 6) 'arithmetic)
(check-expect(sequence-type 2 0 0 0) 'geometric)
(check-expect(sequence-type 7 3 0 9) 'neither)
(check-expect(sequence-type 1 3 5 9) 'neither)
(check-expect(sequence-type -2 -4 -8 -16) 'geometric)



;; (geo-detection a b c d) consumes 4 numbers a, b, c, d and determines whether the last
;; 3 numbers are all 0s. If they are, it is a geometric sequence.


;; Example
(check-expect(geo-detection 2 0 0 0) true)
(check-expect(geo-detection 2 0 1 0) false)


;; geo-detection: Num Num Num Num -> Bool


(define (geo-detection a b c d)
  (cond [(= b c d 0) true]
        [else false]))



;; (zero-detection a b c d) consumes 4 numbers a, b, c, d and determines whether any
;; of the numbers are zeroes.


;; Example
(check-expect(zero-detection 0 3 5 0) true)
(check-expect(zero-detection 1 3 5 100) false)


;; zero-detection: Num Num Num Num -> Bool


(define (zero-detection a b c d)
  (cond [(or (= a 0)(= b 0) (= c 0) (= d 0)) true]
        [else false]))
