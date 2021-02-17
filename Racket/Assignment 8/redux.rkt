;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname redux) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 08, Problem 2a
;; ***************************************************

;; (parity bin-string) consumes a string and returns the symbol ’odd
;; if the number of 1’s in the string is odd, or the symbol ’even
;; if the number of 1’s in the string is even.

;; Examples
(check-expect (parity "110101") 'even)
(check-expect (parity "1110011") 'odd)
(check-expect (parity "0110011") 'even)

;; parity: Str -> Sym
;;   requires string to contain only 0s or 1s
(define (parity bin-string)
  (cond [(even? (foldr (lambda (first-num rror-num) (add1 rror-num)) 0 (filter (lambda (first-char)
                                                                                 (cond [(char=? first-char #\1) true]
                                                                                       [else false])) (string->list bin-string)))) 'even]
        [else 'odd]))
  
;; Tests
(check-expect(parity "") 'even)
(check-expect (parity "0") 'even)
(check-expect (parity "1") 'odd)
(check-expect (parity "010011") 'odd)


;; ***************************************************
;; Problem 2b
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
  (map (lambda (word-in-list)
         (cond [(string=? word-in-list change-word) wanted-word]
               [else word-in-list])) listof-strings))

;; Tests
(check-expect (replace-word "hello" "bye" empty) empty)
(check-expect
(replace-word "exam" "assessment" (cons "exam"(cons "exam"(cons "exam" empty))))
(cons "assessment" (cons "assessment" (cons "assessment" empty))))
(check-expect
(replace-word "exam" "exam" (cons "exam"(cons "content"(cons "assignment" empty))))
(cons "exam" (cons "content" (cons "assignment" empty))))


;; ***************************************************
;; Problem 2c
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
  (filter (lambda (num-in-list)
            (cond [(= num-in-list 0) false]
                  [(= (remainder n num-in-list) 0) true]
                  [else false])) (build-list n (lambda (num) num))))

;; Tests
(check-expect (all-factors 2) (cons 1 empty))
(check-expect (all-factors 3) (cons 1 empty))
(check-expect (all-factors 17) (cons 1 empty))
(check-expect (all-factors 32)
               (cons 1 (cons 2 (cons 4 (cons 8 (cons 16 empty))))))


;; ***************************************************
;; Problem 2d
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
  (map (lambda (first-nums)
         (local [;; (average lst-nums) consumes a list of numbers, lst-nums,
                 ;; and outputs the average of all the numbers.
                 ;; average: (listof Num) -> Num
                 (define (average lst-nums)
                   (/ (foldr + 0 nums)
                      (foldr (lambda (x y) (add1 y)) 0 nums)))]
                 (cond [(< first-nums (average nums)) 'below-mean]
                       [(> first-nums (average nums)) 'above-mean]
                       [(= first-nums (average nums)) 'mean]))) nums))
          
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