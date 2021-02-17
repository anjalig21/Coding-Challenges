;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname vector) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 04, Problem 2a
;; ***************************************************

;; (euclidean-norm v) consumes a vector v (i.e. a list of numbers)
;; and produces the Euclidean Norm of v.

;; Examples
(check-within (euclidean-norm (list 3 4)) 5 0.01)
(check-within (euclidean-norm (list 1 2)) 2.24 0.01)

;; euclidean-norm: (listof Num) -> Num
(define (euclidean-norm v)
  (sqrt (sumof-squares v)))

;; Tests
(check-within (euclidean-norm empty) 0 0.01)
(check-within (euclidean-norm (list 1.4 2 2.8 0)) 3.71 0.01)
(check-within (euclidean-norm (list -1.4 2 -2.8 0)) 3.71 0.01)
(check-within (euclidean-norm (list 0 2 0 0)) 2 0.01)


;; (sumof-squares listof-num) consumes a list of numbers, listof-num,
;; and outputs the sum of each number squared in the list

;; Examples
(check-expect (sumof-squares (list 1 2)) 5)
(check-expect (sumof-squares (list 0 2)) 4)
(check-expect (sumof-squares (list 0 0)) 0)
(check-expect (sumof-squares (list 1.2 2.5)) 7.69)

;; sumof-squares: (listof Num) -> Num
(define (sumof-squares listof-num)
  (cond [(empty? listof-num) 0]
        [else (+ (sqr (first listof-num)) (sumof-squares (rest listof-num)))]))


;; ***************************************************
;; Problem 2b
;; ***************************************************

;; (unit-vector v) consumes a vector, v, (i.e. a list of numbers)
;; of positive length and produces its aligned unit vector.

;; Examples
(check-within (unit-vector (list 3 4)) (list 0.6 0.8) 0.01)
(check-within (unit-vector (list 1 2)) (list 0.45 0.89) 0.01)

;; unit-vector: (listof Num) -> (listof Num)
;;  requires v to be of positive length
;;  requires v to not be a zero vector
(define (unit-vector v)
  (unit-vector-recursion v v))

;; Tests
(check-within (unit-vector (list 0 5)) (list 0 1) 0.01)
(check-within (unit-vector (list -4 5)) (list -0.63 0.79) 0.01)
(check-within (unit-vector (list -3 -4)) (list -0.6 -0.8) 0.01)


;; (unit-vector-recursion lon1 lon2) consumes 2 list of numbers,
;; lon1 and lon2, where lon2 stays constant, and produces its
;; aligned unit vector.

;; Examples
(check-within (unit-vector-recursion (list 3 4) (list 3 4)) (list 0.6 0.8) 0.01)
(check-within (unit-vector-recursion (list 0 4) (list 0 4)) (list 0 1) 0.01)

;; unit-vector-recursion: (listof Num) (listof Num) -> (listof Num)
(define (unit-vector-recursion v v-constant)
  (cond [(empty? v) empty]
        [else (cons (/ (first v) (euclidean-norm v-constant))
                    (unit-vector-recursion (rest v) v-constant))]))


;; ***************************************************
;; Problem 2c
;; ***************************************************

;; (cos-between a b) consumes two non zero vectors, a and b,
;; of positive length and produces the cosine of the angle
;; between a and b.

;; Examples
(check-within (cos-between (list 3 4) (list 0 6)) 0.8 0.01)
(check-within (cos-between (list 5 6 -6) (list 7 8 9)) 0.21 0.01)

;; cos-between: (listof Num) (listof Num) -> Num
;;  requires a and b to be of positive length
;;  requires a and b to be the same degree n
(define (cos-between a b)
  (dot-product (unit-vector a) (unit-vector b)))

;; Tests
(check-within (cos-between (list 3 4) (list 5 6)) 0.99 0.01)
(check-within (cos-between (list -3 -4) (list -5 -6)) 0.99 0.01)
(check-within (cos-between (list -3 -4 5 6 7) (list -5 -6 6 7 8)) 0.99 0.01)


;; (dot-product a b) consumes two list of numbers, a and b,
;; with the same length and produces their dot product.

;; Examples
(check-expect (dot-product (list 1 2) (list 1 2)) 5)
(check-expect (dot-product (list 10 10) (list 10 10)) 200)

;; dot-product: (listof Num) (listof Num) -> Num
(define (dot-product a b)
  (cond [(empty? a) 0]
        [else (+ (* (first a) (first b))
                 (dot-product (rest a) (rest b)))]))