;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 01, Problem 5a
;; ***************************************************


;; (leap-year? x) consumes a number that represents
;; a year and outputs true if the year is a leap year


;; Examples
(check-expect(leap-year? 1900) false)
(check-expect(leap-year? 1904) true)


;; leap-year?: Nat -> Bool
;;     requires year > 0


(define (leap-year? x)
  (cond [(= (remainder x 400)0) true]
        [(= (remainder x 100)0) false]
        [(= (remainder x 4)0) true]
        [else false]))


;; Tests
(check-expect(leap-year? 2000) true)
(check-expect(leap-year? 2020) true)
(check-expect(leap-year? 2017) false)
(check-expect(leap-year? 1500) false)



;; ***************************************************
;; Problem 5b
;; ***************************************************


;; Minimum Valid Date Characters
(define minchars 5)


;; Months
(define Jan 1)
(define Feb 2)
(define Mar 3)
(define Apr 4)
(define May 5)
(define Jun 6)
(define Jul 7)
(define Aug 8)
(define Sep 9)
(define Oct 10)
(define Nov 11)
(define Dec 12)


;; Unusual Dates (September 3-13 1752 (inclusive))
(define unusual-year 1752)
(define unusual-dayrange1 3)
(define unusual-dayrange2 13)


;; (valid-date? y) consumes a number y, representing a date,
;; and outputs true if the date is valid otherwise outputs false


;; Examples
(check-expect(valid-date? 123456789) false)
(check-expect(valid-date? 20200922) true)
(check-expect (valid-date? 19040230) false)


;; valid-date?: Nat -> Bool
;;   requires year > 0, month > 0, day > 0


(define (valid-date? y)
  (cond [(<(string-length(number->string y)) minchars) false]
        
        [(and (= (year y) unusual-year) (= (month y) Sep)
              (and (>= (day y) unusual-dayrange1) (<= (day y) unusual-dayrange2))) false]
        
        [(and (= (month y) Feb) (equal? (leap-year? (year y)) true)
              (and (> (day y) 0) (< (day y) 30))) true]
        
        [(and (= (month y) Feb) (and (> (day y) 0) (< (day y) 29))) true]
        
        [(and (equal?(check-month31days y)true) (and (> (day y) 0) (< (day y) 32))) true]
        
        [(and (equal?(check-month30days y)true) (and (> (day y) 0) (< (day y) 31))) true]
        
        [else false])) 


;; Tests
(check-expect(valid-date? 30313) true)
(check-expect(valid-date? 17520903) false)
(check-expect(valid-date? 17520913) false)
(check-expect(valid-date? 20200229) true)
(check-expect(valid-date? 20210228) true)
(check-expect(valid-date? 01021) false)



;; (check-month30days y) consumes a date, y, and outputs true if the month has 30 days

;; Examples
(check-expect (check-month30days 20201121) true)

;; check-month30days: Nat -> Bool

(define (check-month30days y)
  (or (= (month y) Apr)
      (= (month y) Jun)
      (= (month y) Sep)
      (= (month y) Nov)))



;; (check-month31days y) consumes a date, y, and outputs true if the month has 31 days
;; and false if the month has 30 days

;; Examples
(check-expect (check-month31days 17520921) false)
(check-expect (check-month31days 17520821) true)

;; check-month31days: Nat -> Bool

(define (check-month31days y)
  (cond [(or (= (month y) Jan)
             (= (month y) Mar)
             (= (month y) May)
             (= (month y) Jul)
             (= (month y) Aug)
             (= (month y) Oct)
             (= (month y) Dec)) true]
        [else false]))



;; (year y) consumes a date, y, and checks what the year is.

;; Examples
(check-expect (year 20200923) 2020)

;; year: Nat -> Bool

(define (year y) (floor(/ y 10000)))



;; (month y) consumes a date, y, and checks what the month is.

;; Examples
(check-expect (month 20200923) 9)

;; month: Nat -> Bool

(define (month y) (floor(/ (modulo y 10000) 100)))



;; (day y) consumes a date, y, and checks what the day is.

;; Examples
(check-expect (day 20200923) 23)

;; month: Nat -> Bool

(define (day y) (modulo y 100))
