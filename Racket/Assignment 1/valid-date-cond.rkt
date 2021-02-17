;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname valid-date-cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 01, Problem 5d
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
(check-expect(valid-date? 20200228) true)
(check-expect(valid-date? 20201031) true)
(check-expect(valid-date? 20151130) true)
(check-expect(valid-date? 20210228) true)
(check-expect(valid-date? 20210928) true)
(check-expect(valid-date? 17520900) false)
(check-expect(valid-date? 17520931) false)
(check-expect(valid-date? 17520930) true)



;; valid-date?: Nat -> Bool
;;   requires year > 0, month > 0, day > 0


(define (valid-date? y)
  (cond [(< (string-length(number->string y)) 5) false]
        
        [(= (year y) unusual-year)
         (cond [(= (month y) Sep)
                (cond [(<= (day y) unusual-dayrange2)
                          (cond[(>= (day y) unusual-dayrange1) false]
                               [(< (day y) 1) false]
                               [else true])]
                      [(> (day y) 30) false]
                      [(> (day y) unusual-dayrange2) true])])]
      
        [(check-month31days y)
                (cond[(>= (day y) 1)
                     (cond [(<= (day y) 31) true]
                           [else false])]
                     [(< (day y) 1) false])]

        [(check-month30days y)
                (cond[(>= (day y) 1)
                     (cond [(<= (day y) 30) true]
                           [else false])]
                     [(< (day y) 1) false])]
       
        [(leap-year? (year y))
         (cond [(= (month y) Feb)
               (cond[(>= (day y) 1)
                     (cond [(<= (day y) 29) true]
                           [else false])]
                    [(< (day y) 1) false])])]

        [(= (month y) Feb)
               (cond[(>= (day y) 1)
                     (cond [(<= (day y) 28) true]
                           [else false])]
                    [(< (day y) 1) false])]
        
        [else false]))



;; Tests
(check-expect (valid-date? 01021) false)
(check-expect (valid-date? 30313) true)
(check-expect (valid-date? 20200229) true)
(check-expect (valid-date? 00000) false)
(check-expect (valid-date? 17520903) false)
(check-expect (valid-date? 17520913) false)
(check-expect (valid-date? 17520902) true)
(check-expect (valid-date? 19040230) false)
(check-expect (valid-date? 20020230) false)
(check-expect (valid-date? 20020433) false)
(check-expect (valid-date? 20020500) false)
(check-expect (valid-date? 20020400) false)
(check-expect (valid-date? 20020200) false)
(check-expect (valid-date? 20200200) false)
(check-expect (valid-date? 20201032) false)



;; (leap-year? x) consumes a number that represents
;; a year and outputs true if the year is a leap year

;; Examples
(check-expect(leap-year? 1900) false)
(check-expect(leap-year? 1904) true)
(check-expect(leap-year? 2000) true)

;; leap-year?: Nat -> Bool

(define (leap-year? x)
  (cond [(= (remainder x 400)0) true]
        [(= (remainder x 100)0) false]
        [(= (remainder x 4)0) true]
        [else false]))



;; (check-month31days y) consumes a date, y, and outputs true if the month has 31 days

;; Examples
(check-expect (check-month31days 20200921) false)
(check-expect (check-month31days 20200821) true)
(check-expect (check-month31days 20200121) true)
(check-expect (check-month31days 20200521) true)
(check-expect (check-month31days 20200721) true)
(check-expect (check-month31days 20201221) true)

;; check-month31days: Nat -> Bool

(define (check-month31days y)
  (cond [(= (month y) Jan) true]
        [(= (month y) Mar) true]
        [(= (month y) May) true]
        [(= (month y) Jul) true]
        [(= (month y) Aug) true]
        [(= (month y) Oct) true]
        [(= (month y) Dec) true]
        [else false]))

        

;; (check-month30days y) consumes a date, y, and outputs true if the month has 30 days

;; Examples
(check-expect (check-month30days 20201121) true)
(check-expect (check-month30days 20200421) true)
(check-expect (check-month30days 20200621) true)

;; check-month30days: Nat -> Bool

(define (check-month30days y)
  (cond [(= (month y) Apr) true]
        [(= (month y) Jun) true]
        [(= (month y) Sep) true]
        [(= (month y) Nov) true]
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