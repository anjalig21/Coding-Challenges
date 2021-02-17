;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname brackets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 09, Problem 2
;; ***************************************************

;; (balanced? bracket-string) consumes a bracket string, bracket-string
;; and determines whether it is balanced.

;; Examples
(check-expect (balanced? "(<)>") false)
(check-expect (balanced? "((<>[])<>)[]") true)
(check-expect (balanced? "") true)

;; balanced?: Str -> Bool
(define (balanced? bracket-string)
  (local [;; (check-balance? stack bracket-list) consumes two list of characters, stack,
          ;; and bracket-string, and then produces true if the list is balanced and
          ;; false otherwise.
          ;; check-balance: (listof Char) (listof Char) -> Bool
          (define (check-balance? stack bracket-list)
            (local [;; (open-bracket? bracket) consumes a character, bracket and
                    ;; produces true it is a open bracket and false otherwise.
                    ;; open-bracket?: Char -> Bool
                    (define (open-bracket? bracket)
                       (or (char=? bracket #\() (char=? bracket #\<) (char=? bracket #\[)))
                    ;; (close-bracket? bracket) consumes a character, bracket and
                    ;; produces true it is a closed bracket and false otherwise.
                    ;; close-bracket Char -> Bool
                     (define (close-bracket? bracket)
                       (or (char=? bracket #\)) (char=? bracket #\>) (char=? bracket #\])))]
              (cond [(and (empty? bracket-list) (empty? stack)) true]
                    [(empty? bracket-list) false]
                    [(open-bracket? (first bracket-list))
                     (check-balance? (cons (first bracket-list) stack) (rest bracket-list))]
                    [(close-bracket? (first bracket-list))
                     (cond [(and (char=? (first bracket-list) #\)) (not (empty? stack)) (char=? (first stack) #\())
                            (check-balance? (rest stack) (rest bracket-list))]
                           [(and (char=? (first bracket-list) #\>) (not (empty? stack)) (char=? (first stack) #\<))
                            (check-balance? (rest stack) (rest bracket-list))]
                           [(and (char=?  (first bracket-list) #\]) (not (empty? stack)) (char=? (first stack) #\[))
                            (check-balance? (rest stack) (rest bracket-list))]
                           [else false])])))]
    (cond [(empty? (string->list bracket-string)) true]
          [else (check-balance? (list (first (string->list bracket-string)))
                                (rest (string->list bracket-string)))])))
 
;; Tests
(check-expect (balanced? "((((((((") false)
(check-expect (balanced? "<<><>") false)
(check-expect (balanced? "<><>>") false)
(check-expect (balanced? "(") false)
(check-expect (balanced? ">") false)
(check-expect (balanced? "[") false)
(check-expect (balanced? "]]]]]") false)
(check-expect (balanced? "(<>[])()[][][]") true)