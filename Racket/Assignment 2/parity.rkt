;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname parity) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 02, Problem 5
;; ***************************************************

;; (parity bin-string) consumes a string and returns the symbol ’odd
;; if the number of 1’s in the string is odd, or the symbol ’even
;; if the number of 1’s in the string is even.

;; parity: Str -> Sym
;;   requires string to contain only 0s or 1s
(define (parity bin-string)
  (cond [(empty? (string->list bin-string)) 'even]
        [(char=? (first (string->list bin-string)) #\0)
         (cond [(even? (parity-recursion (rest(string->list bin-string)))) 'even]
                                                               [else 'odd])]
        [else (cond [(even? (+ 1 (parity-recursion (rest(string->list bin-string))))) 'even]
                    [else 'odd])]))
        
;; Tests
(check-expect(parity "") 'even)
(check-expect (parity "0") 'even)
(check-expect (parity "1") 'odd)
(check-expect (parity "010011") 'odd)



;; (parity-recursion listof-binary) consumes a list of binary numbers
;; and returns the number of 1s in the list

;; Examples
(check-expect(parity-recursion (cons #\0 (cons #\0 (cons #\1 empty)))) 1)
(check-expect(parity-recursion (cons #\1 (cons #\0 (cons #\1 empty)))) 2)

;; parity-recursion: (listof-Num) -> Nat
;;   requires string to contain only 0s or 1s
(define (parity-recursion listof-binary)
  (cond [(empty? listof-binary) 0]
        [(char=? (first listof-binary) #\0) (parity-recursion (rest listof-binary))]
        [else (+ 1 (parity-recursion (rest listof-binary)))]))

