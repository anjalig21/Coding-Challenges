;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mean-relative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 02, Problem 6
;; ***************************************************

;; (mean-relative nums) consumes a list of integers, nums, and produces a list of symbols
;; where each symbol in the produced list is either ’below-mean, ’above-mean,
;; or ’mean if the number at location in the list was below the mean of the list, above the mean
;; of the list, or equal to the mean of the list respectively.

;; Examples
(check-expect(mean-relative (cons 5 (cons 7 (cons 9 (cons 12 empty)))))
             (cons 'below-mean (cons 'below-mean (cons 'above-mean (cons 'above-mean empty)))))
(check-expect(mean-relative (cons 100 (cons 10 empty))) (cons 'above-mean (cons 'below-mean empty)))

;; mean-relative: (listof-Int) -> (listof-Sym)
(define (mean-relative nums)
  (cond [(empty? nums) empty]
        [(empty? (rest nums)) (cons 'mean empty)]
        [else (mean-relative-recursion nums nums)]))

;; Tests
(check-expect(mean-relative (cons 5 (cons 10 (cons 9 (cons 12 empty)))))
             (cons 'below-mean (cons 'above-mean (cons 'mean (cons 'above-mean empty)))))
(check-expect(mean-relative empty) empty)
(check-expect(mean-relative (cons 0 empty)) (cons 'mean empty))
(check-expect(mean-relative (cons 100 empty)) (cons'mean empty))
(check-expect(mean-relative (cons -4 (cons 7 (cons 1 (cons 12 empty)))))
             (cons 'below-mean (cons 'above-mean (cons 'below-mean (cons 'above-mean empty)))))
(check-expect(mean-relative (cons 0 (cons 16 (cons 0 (cons 0 empty)))))
             (cons 'below-mean (cons 'above-mean (cons 'below-mean (cons 'below-mean empty)))))



;; (mean-relative-recursion nums changing-list) consumes  a list of integers, nums, and another list
;; of integers, changing-list, and produces a list of symbols
;; where each symbol in the produced list is either ’below-mean, ’above-mean,
;; or ’mean if the number at location in the list was below the mean of the list, above the mean
;; of the list, or equal to the mean of the list respectively.

;; Examples
(check-expect(mean-relative-recursion (cons 5 (cons 7 (cons 9 (cons 12 empty))))
                             (cons 5 (cons 7 (cons 9 (cons 12 empty)))))
             (cons 'below-mean (cons 'below-mean (cons 'above-mean (cons 'above-mean empty)))))
(check-expect(mean-relative-recursion (cons 100 (cons 10 empty)) (cons 100 (cons 10 empty)))
             (cons 'above-mean (cons 'below-mean empty)))
(check-expect(mean-relative-recursion empty empty) empty)

;; mean-relative-recursion: (listof-Int) (listof-Int) -> (listof-Sym)
(define (mean-relative-recursion nums changing-list)
  (cond [(empty? changing-list) empty] 
        [else (cons (checkrelative-mean (first changing-list) nums)
                    (mean-relative-recursion nums (rest changing-list)))]))



;; (checkreative-mean num mean-number) consumes a number, num, and
;; a list of numbers, nums, and checks if num is greater than, equal to or less than the
;; mean of the list of numbers. It then outputs the appropriate symbol.

;; Examples
(check-expect(checkrelative-mean 4 (cons 5 (cons 7 empty))) 'below-mean)
(check-expect(checkrelative-mean 6 (cons 5 (cons 7 empty))) 'mean)
(check-expect(checkrelative-mean 7 (cons 5 (cons 7 empty))) 'above-mean)

;; checkrelative-mean: Num Num -> Symbol
(define (checkrelative-mean num nums)
             (cond [(> num (mean nums)) 'above-mean]
                   [(< num (mean nums)) 'below-mean]
                   [(= num (mean nums)) 'mean]))

        
        
;; (mean nums) consumes a list of integers, nums,
;; and outputs the mean of the integers

;; Examples
(check-expect(mean (cons 5 (cons 7 (cons 9 (cons 12 empty))))) 8.25)
(check-expect(mean (cons 1 (cons 1 (cons 1 (cons 1 empty))))) 1)

;; mean: (listof-Int) -> Num
(define (mean nums) (/ (sumof-nums nums) (length nums)))

                     

;; (sumof-nums) consumes a list of integers, nums,
;; and outputs the total of the integers

;; Examples
(check-expect(sumof-nums (cons 5 (cons 7 (cons 9 (cons 12 empty))))) 33)
(check-expect(sumof-nums (cons 5 (cons 1 (cons 1 (cons 12 empty))))) 19)
(check-expect(sumof-nums (cons -5 (cons 40 (cons 5 (cons 5 empty))))) 45)
        
;; sumof-nums: (listof-Int) -> Int
(define (sumof-nums nums)
  (cond [(empty? nums) 0]
        [else (+ (first nums) (sumof-nums (rest nums)))]))




