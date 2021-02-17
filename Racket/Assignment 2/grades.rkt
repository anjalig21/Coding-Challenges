;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 02, Problem 3
;; ***************************************************

;; (cs135-grade self-check assign mt1 mt2 final) consumes
;; 5 numbers: self-check, assignment,
;; mid-term1, mid-term2 and final grades
;; It then produces the final grade received in CS135 out of 100.

;; Examples
(check-expect(cs135-grade 0.8 0.6 0.4 0.9 0.95) 68.3) 
(check-expect(cs135-grade 0.8 0.6 0.4 0.9 0) 46)

;; cs135-grade: Num Num Num Num Num -> Num
;;      requires: self-check, assign, mt1, mt2, final
;;      to be greater than or equal 0 but less than or equal to 1
(define (cs135-grade self-check assign mt1 mt2 final)
  (cond [(or (< (exam mt1 mt2 final) 0.50) (< assign 0.50))
         (* (min (normal_calc self-check assign mt1 mt2 final) 0.46) 100)]
        [else (* (normal_calc self-check assign mt1 mt2 final) 100)]))

;; Tests
(check-expect(cs135-grade 0 0 0 0 0) 0)
(check-expect(cs135-grade 1 1 1 1 1) 100)
(check-expect(cs135-grade 0.01 0.01 0.01 0.01 0.01) 1)



;; (normal_calc self-check assign mt1 mt2 final) consumes
;; 5 numbers: self-check, assignment,
;; mid-term1, mid-term2 and final grades
;; It then produces the actual mark.

;; Examples
(check-expect(normal_calc 1 0.9 0.9 0.85 0.9) 0.9065)

;; normal_calc: Num Num Num Num Num -> Num
;;      requires: self-check, assign, mt1, mt2, final
;;      to be greater than or equal 0 but less than or equal to 1
(define (normal_calc self-check assign mt1 mt2 final)
  (+ (* self-check .10) (* assign .60) (* mt1 .07) (* mt2 .07) (* final .16)))



;; (exam mt1 mt2 final) consumes 3 numbers,
;; mid-term1, mid-term2 and final. It then produces
;; the exam mark total out of 100.

;; Examples
(check-expect(exam 1 0 0.95) 0.74)

;; exam: Num Num Num -> Num
;;      requires: mt1, mt2, final
;;      to be greater than or equal 0 but less than or equal to 1
(define (exam mt1 mt2 final)
  (/ (+ (* mt1 .07) (* mt2 .07) (* final .16)) .30))





