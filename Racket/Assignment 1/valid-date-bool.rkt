;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date-bool) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 01, Problem 5c
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

;; valid-date?: Nat -> Bool
;;   requires year > 0, month > 0, day > 0


(define (valid-date? y)
  (or
   (and (false? (= (year y) unusual-year)) (= (month y) Sep)
        (and (>=(day y) 1) (<= (day y) 30)))
   
   (and (= (year y) unusual-year) (= (month y) Sep)
        (or (and (>= (day y) 1) (< (day y) unusual-dayrange1)) (and (> (day y) unusual-dayrange2) (<= (day y) 30))))
   
   (and (>= (string-length(number->string y)) minchars) (leap-year? (year y)) (= (month y) Feb)
        (and (>= (day y) 1) (<= (day y) 29)))
   
   (and (>= (string-length(number->string y)) minchars) (false? (leap-year? (year y))) (= (month y) Feb)
        (and (>= (day y) 1) (<= (day y) 28)))
   
   (and (>= (string-length(number->string y)) minchars) (check-month31days y)
        (and (>=(day y) 1) (<= (day y) 31)))
   
   (and (>= (string-length(number->string y)) minchars) (check-month30days y)
        (and (>=(day y) 1) (<= (day y) 30)))))



;; Tests
(check-expect(valid-date? 01021) false)
(check-expect(valid-date? 30313) true)
(check-expect(valid-date? 20200229) true)
(check-expect(valid-date? 00000) false)
(check-expect(valid-date? 17520903) false)
(check-expect(valid-date? 17520913) false)
(check-expect(valid-date? 17520930) true)



;; (leap-year? x) consumes a number that represents
;; a year and outputs true if the year is a leap year

;; leap-year?: Nat -> Bool

(define (leap-year? x)
  (or (= (remainder x 400)0)
      (and (= (remainder x 4)0) (false?(= (remainder x 100)0)))))



;; (check-month31days y) consumes a date, y, and outputs true if the month has 31 days

;; Examples
(check-expect (check-month31days 20200921) false)
(check-expect (check-month31days 20200821) true)

;; check-month31days: Nat -> Bool

(define (check-month31days y)
  (or (= (month y) Jan)
      (= (month y) Mar)
      (= (month y) May)
      (= (month y) Jul)
      (= (month y) Aug)
      (= (month y) Oct)
      (= (month y) Dec)))



;; (check-month30days y) consumes a date, y, and outputs true if the month has 30 days
;; excluding September

;; Examples
(check-expect (check-month30days 20201121) true)

;; check-month30days: Nat -> Bool

(define (check-month30days y)
  (or (= (month y) Apr)
      (= (month y) Jun)
      (= (month y) Nov)))



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