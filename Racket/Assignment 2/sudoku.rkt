;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 02, Problem 7
;; ***************************************************

;; (sudoku-valid? listof-nums) consumes a list of numbers, nums, and produces
;; true if the list of numbers contains only each of the numbers one through
;; nine exactly once and nothing else, or produces false otherwise.

;; Examples
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
(cons 9 (cons 8 (cons 7 (cons 6 empty)))))))))) true)
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
(cons 10 (cons 8 (cons 7 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? empty) false)

;; sudoku-vaid? (listof-Num) -> Bool
(define (sudoku-valid? listof-nums)
  (cond [(empty? listof-nums) false]
        [(false? (= (length listof-nums) 9)) false]
        [(and true (check-bound listof-nums)
              (first-term listof-nums listof-nums)
              (second-term (rest listof-nums)
                           (rest listof-nums))
              (third-term (rest (rest listof-nums))
                          (rest (rest listof-nums)))
              (fourth-term (rest (rest (rest listof-nums)))
                           (rest (rest (rest listof-nums))))
              (fifth-term (rest (rest (rest (rest listof-nums))))
                          (rest (rest (rest (rest listof-nums)))))
              (sixth-term (rest (rest (rest (rest (rest listof-nums)))))
                          (rest (rest (rest (rest (rest listof-nums))))))
              (seventh-term (rest (rest (rest (rest (rest (rest listof-nums))))))
                            (rest (rest (rest (rest (rest (rest listof-nums)))))))
              (eigth-term (rest (rest (rest (rest (rest (rest (rest listof-nums)))))))
                          (rest (rest (rest (rest (rest (rest (rest listof-nums))))))))) true]
        [else false ]))

;; Tests
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
(cons 9 (cons 7 (cons 8 (cons 8 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
(cons 5 (cons 8 (cons 7 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
(cons 6 (cons 6 (cons 7 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
(cons 9 (cons 4 (cons 7 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 2 (cons 4 (cons 5
(cons 9 (cons 8 (cons 7 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 3 (cons 4 (cons 3
(cons 9 (cons 8 (cons 7 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 1 (cons 1 (cons 3 (cons 4 (cons 5
(cons 9 (cons 8 (cons 7 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 1 (cons 1 (cons 2 (cons 3 (cons 4
(cons 5(cons 9 (cons 8 (cons 7 (cons 6 empty))))))))))) false)
(check-expect (sudoku-valid? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5
(cons 9 (cons 8 (cons 8 (cons 6 empty)))))))))) false)
(check-expect (sudoku-valid? (cons 6 empty)) false)




;; (first-term listof-nums nums) consumes 2 list of numbers, listof-nums and nums
;; and checks if the first term of listof-nums has any duplicates.
;; If there are duplicates it outputs false, otherwise true.

;; Examples
(check-expect(first-term (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))
                         (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))) false)

;; first-term: (listof-Num) (listofNum) -> Bool
(define (first-term listof-nums nums)
  (cond [(empty? (rest nums)) true]
        [(= (first listof-nums) (first (rest nums))) false]
        [else (first-term listof-nums (rest nums))]))



;; (second-term listof-nums nums) consumes 2 list of numbers, listof-nums and nums
;; and checks if the second term of listof-nums has any duplicates.
;; If there are duplicates it outputs false, otherwise true.

;; Examples
(check-expect(second-term (cons 5(cons 6(cons 7(cons 8(cons 9(cons 1 empty))))))
                         (cons 5(cons 6(cons 7(cons 8(cons 9(cons 1 empty))))))) true)

;; second-term: (listof-Num) (listofNum) -> Bool
(define (second-term listof-nums nums)
  (cond [(empty? (rest nums)) true]
        [(= (first listof-nums) (first (rest nums))) false]
        [else (second-term listof-nums (rest nums))]))



;; (third-term listof-nums nums) consumes 2 list of numbers, listof-nums and nums
;; and checks if the third term of listof-nums has any duplicates.
;; If there are duplicates it outputs false, otherwise true.

;; Examples
(check-expect(third-term (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))
                         (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))) false)

;; third-term: (listof-Num) (listofNum) -> Bool
(define (third-term listof-nums nums)
  (cond [(empty? (rest nums)) true]
        [(= (first listof-nums) (first (rest nums))) false]
        [else (third-term listof-nums (rest nums))]))



;; (fourth-term listof-nums nums) consumes 2 list of numbers, listof-nums and nums
;; and checks if the fourth term of listof-nums has any duplicates.
;; If there are duplicates it outputs false, otherwise true.

;; Examples
(check-expect(fourth-term (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))
                         (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))) false)

;; fourth-term: (listof-Num) (listofNum) -> Bool
(define (fourth-term listof-nums nums)
  (cond [(empty? (rest nums)) true]
        [(= (first listof-nums) (first (rest nums))) false]
        [else (fourth-term listof-nums (rest nums))]))



;; (fifth-term listof-nums nums) consumes 2 list of numbers, listof-nums and nums
;; and checks if the fifth term of listof-nums has any duplicates.
;; If there are duplicates it outputs false, otherwise true.

;; Examples
(check-expect(fifth-term (cons 5(cons 6(cons 7(cons 8(cons 5 empty)))))
                         (cons 5(cons 6(cons 7(cons 8(cons 5 empty)))))) false)

;; fifth-term: (listof-Num) (listofNum) -> Bool
(define (fifth-term listof-nums nums)
  (cond [(empty? (rest nums)) true]
        [(= (first listof-nums) (first (rest nums))) false]
        [else (fifth-term listof-nums (rest nums))]))



;; (sixth-term listof-nums nums) consumes 2 list of numbers, listof-nums and nums
;; and checks if the sixth term of listof-nums has any duplicates.
;; If there are duplicates it outputs false, otherwise true.

;; Examples
(check-expect(sixth-term (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))
                         (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))) false)

;; sixth-term: (listof-Num) (listofNum) -> Bool
(define (sixth-term listof-nums nums)
  (cond [(empty? (rest nums)) true]
        [(= (first listof-nums) (first (rest nums))) false]
        [else (sixth-term listof-nums (rest nums))]))



;; (seventh-term listof-nums nums) consumes 2 list of numbers, listof-nums and nums
;; and checks if the seventh term of listof-nums has any duplicates.
;; If there are duplicates it outputs false, otherwise true.

;; Examples
(check-expect(seventh-term (cons 5(cons 6(cons 7(cons 8(cons 9(cons 100 empty))))))
                         (cons 5(cons 6(cons 7(cons 8(cons 9(cons 100 empty))))))) true)

;; seventh-term: (listof-Num) (listofNum) -> Bool
(define (seventh-term listof-nums nums)
  (cond [(empty? (rest nums)) true]
        [(= (first listof-nums) (first (rest nums))) false]
        [else (seventh-term listof-nums (rest nums))]))



;; (eigth-term listof-nums nums) consumes 2 list of numbers, listof-nums and nums
;; and checks if the eighth term of listof-nums has any duplicates.
;; If there are duplicates it outputs false, otherwise true.

;; Examples
(check-expect(eigth-term (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))
                         (cons 5(cons 6(cons 7(cons 8(cons 9(cons 5 empty))))))) false)

;; eigth-term: (listof-Num) (listofNum) -> Bool
(define (eigth-term listof-nums nums)
  (cond [(empty? (rest nums)) true]
        [(= (first listof-nums) (first (rest nums))) false]
        [else (eigth-term listof-nums (rest nums))]))



;; (check-bound listof-nums) consumes a list of 9 numbers,
;; listof-nums and outputs true if all the terms are
;; between 1-9 (inclusive) and false otherwise.

;; Examples
(check-expect(check-bound (cons 10 (cons 3 (cons 1 (cons 3 (cons 4 (cons 6 (cons 8 (cons 3 (cons 1 empty)))))))))) false)
(check-expect(check-bound (cons 9 (cons 3 (cons 1 (cons 3 (cons 4 (cons 6 (cons 8 (cons 3 (cons 1 empty)))))))))) true)
(check-expect(check-bound (cons 9 (cons 3 (cons 1 (cons 3 (cons 4 (cons 6 (cons 8 (cons 3 (cons 0 empty)))))))))) false)

;; check-bound: (listof-Num) -> Bool
(define (check-bound listof-nums)
  (cond [(empty? listof-nums) true]
        [(or (> (first listof-nums) 9) (< (first listof-nums) 1)) false]
        [else (check-bound (rest listof-nums))]))








