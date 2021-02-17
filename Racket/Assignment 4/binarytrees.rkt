;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname roster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 06, Problem 1a
;; ***************************************************

;; =============================================================================
;; struct and data definitions For Q1
;; =============================================================================

;; A StudentID is a Nat with at most 8 digits (i.e. 0 <= id <= 99999999)

;; A Grade is one of:
;; * false
;; * Nat
;;   Requires: Nat is between 0 and 100 (inclusive)

(define-struct student (id name grade))
;; A Student is a (make-student StudentID Str Grade)


(define-struct rnode (student left right))
;; A Roster Node (RN) is a (make-rnode Student Roster Roster)
;; Requires: all students in the left subtree have an ID < student's ID
;;           all students in the right subtree have an ID > student's ID

;; A Roster is one of 
;; * empty
;; * RN


;; =============================================================================
;; constants used in Q1 examples
;; =============================================================================

(define beth (make-student 12345678 "Beth" 96))
(define beth/new (make-student 12345678 "Hanna" 96))
(define beth2 (make-student 02345678 "Beth" 96))
(define beth3 (make-student 00345678 "Beth" 96))
(define beth4 (make-student 42345678 "Beth" 96))
(define beth5 (make-student 92345678 "Beth" 96))
(define jenny (make-student 08675309 "Jenny" 81))
(define john1 (make-student 48975311 "John" 95))
(define jenny/new (make-student 08675309 "Jen" 81))
(define john2 (make-student 20488192 "John" false))

(define sample-roster
  (make-rnode beth ; root
              (make-rnode jenny empty empty)   ; left child
              (make-rnode john1 empty empty))) ; right child

(define sample-roster-2
  (make-rnode beth 
              (make-rnode jenny/new empty empty)
              (make-rnode john1
                          (make-rnode john2 empty empty)
                          empty)))

(define sample-roster-3
  (make-rnode john2 
              (make-rnode john2 empty empty)
              (make-rnode john2 
                          (make-rnode john2 empty empty)
                          empty)))

(define sample-roster-4
  (make-rnode beth 
              (make-rnode beth2
                          (make-rnode beth3 empty empty) empty)
              (make-rnode beth4 empty
                          (make-rnode beth5 empty empty))))

(define sample-roster-5
  (make-rnode beth/new 
              (make-rnode jenny empty empty)   
              (make-rnode john1 empty empty)))

;; (find-student studentid-num roster) consumes a StudentID, student-id-num,
;; and a Roster, roster. It then produces the matching student. If the
;; roster does not contain a student with that ID, the function
;; produces false.

;; find-student: StudentID Roster -> (anyof Student Bool)
(define (find-student studentid-num roster)
  (cond [(empty? roster) false]
        [(= studentid-num (student-id(rnode-student roster))) (rnode-student roster)]
        [(< studentid-num (student-id(rnode-student roster))) (find-student studentid-num (rnode-left roster))]
        [(> studentid-num (student-id(rnode-student roster))) (find-student studentid-num (rnode-right roster))]))

;; Tests
(check-expect (find-student 08675309 sample-roster-2) jenny/new)
(check-expect (find-student 48975311 sample-roster-2) john1)

;; ***************************************************
;; Problem 1b
;; ***************************************************

;; (class-average roster) consumes a Roster, roster, and produces the
;; class average (mean).

;; class-average: Roster -> Num
(define (class-average roster)
  (cond [(all-false? roster) 'N/A]
        [else (/ (sumof-grades roster) (count-students roster))]))

;; Tests
(check-expect (class-average sample-roster-3) 'N/A)
(check-expect (class-average empty) 'N/A)


;; (sumof-grades roster) consumes a Roster, roster,
;; and outputs the sum of the students' grades.

;; Examples
(check-expect (sumof-grades sample-roster) 272)
(check-expect (sumof-grades sample-roster-2) 272)

;; sumof-grades: Roster -> Num
(define (sumof-grades roster)
  (cond [(empty? roster) 0]
        [(boolean? (student-grade (rnode-student roster))) 0]
        [else (+ (student-grade (rnode-student roster))
                 (sumof-grades (rnode-left roster))
                 (sumof-grades (rnode-right roster)))]))


;; (count-students roster) consumes a Roster, roster
;; and counts the number of students in a roster
;; with a grade as a number.

;; Examples
(check-expect (count-students sample-roster) 3)
(check-expect (count-students sample-roster-2) 3)

;; count-students: Roster -> Nat
(define (count-students roster)
  (cond [(empty? roster) 0]
        [(boolean? (student-grade (rnode-student roster))) 0]
        [else (+ 1 (count-students (rnode-left roster))
                 (count-students (rnode-right roster)))]))


;; (all-false? roster) consumes a Roster, roster,
;; and outputs true if the grades of all the students
;; are false and false otherwise.

;; Examples
(check-expect (all-false? sample-roster-3) true)
(check-expect (all-false? sample-roster) false)

;; all-false?: Roster -> Bool
(define (all-false? roster)
  (cond [(empty? roster) true]
        [(and (boolean? (student-grade (rnode-student roster)))
              (all-false? (rnode-left roster))
                          (all-false? (rnode-right roster))) true]
        [else false]))


;; ***************************************************
;; Problem 1c
;; ***************************************************

;; (find-student/name name roster) that consumes a string, name, and a
;; Roster, roster, and produces a list of students with that exact name
;; If there are multiple students, they will be ordered by their student ID.

;; Examples
(check-expect (find-student/name "Beth" sample-roster) (list beth))
(check-expect (find-student/name "Dan" sample-roster) empty)

;; find-student/name: Str Roster -> (listof Student)
(define (find-student/name name roster)
  (cond [(empty? roster) empty]
        [(string=? name (student-name (rnode-student roster)))
         (append (find-student/name name (rnode-left roster)) (list (rnode-student roster)) (find-student/name name (rnode-right roster)))]
        [else (append (find-student/name name (rnode-left roster)) (find-student/name name (rnode-right roster)))]))

;; Tests
(check-expect (find-student/name "Beth" sample-roster-4) (list beth3 beth2 beth beth4 beth5))
(check-expect (find-student/name "John" sample-roster-2) (list john2 john1))



;; ***************************************************
;; Problem 1d
;; ***************************************************

;; (add-students listof-updates roster) consumes a (listof (list StudentID Str)),
;; list-updates, and a Roster, roster. The function produces a new Roster equal
;; to the consumed Roster but with each new student from the list added.
;; If a StudentID was already in the Roster then their name is updated.

;; Examples
(check-expect (add-students (list (list 20488192 "John") (list 8675309 "Jen")) sample-roster)
              sample-roster-2)
(check-expect (add-students empty sample-roster) sample-roster)

;; add-students: (listof (list StudentID Str)) Roster -> Roster
(define (add-students listof-updates roster)
  (cond [(empty? listof-updates) roster]
        [(empty? roster) (add-students (rest listof-updates)
                                       (make-rnode (make-student (first (first listof-updates))
                                                                 (second (first listof-updates)) false)
                                                   empty empty))]
        [else (add-students (rest listof-updates) (add-node (list (first listof-updates)) roster))]))

;; Tests
(check-expect (add-students (list (list 2088884 "Johnny")) empty)
              (make-rnode (make-student 2088884 "Johnny" #false) empty empty))
(check-expect (add-students empty empty) empty)


;; (add-node listof-updates roster) consumes a (listof (list StudentID Str)),
;; list-updates, and a Roster, roster. It then produces a new Roster equal
;; to the consumed Roster but with the first new student from the list added.
;; If a StudentID was already in the Roster then their name is updated.

;; Examples
(check-expect (add-node empty empty) empty)
(check-expect (add-node (list (list 12345678 "Hanna")) sample-roster)
              sample-roster-5)

;; add-node: (listof (list StudentID Str)) Roster -> Roster
(define (add-node listof-updates roster)
  (cond [(empty? listof-updates) roster]
        
        [(empty? roster) (make-rnode (make-student (first (first listof-updates))
                                                   (second (first listof-updates)) false) empty empty)]
        [(> (first (first listof-updates)) (student-id (rnode-student roster))) (make-rnode (rnode-student roster)
                                                                                            (rnode-left roster)
                                                                                            (add-node listof-updates (rnode-right roster)))]
        [(< (first (first listof-updates)) (student-id (rnode-student roster))) (make-rnode (rnode-student roster)
                                                                                            (add-node listof-updates (rnode-left roster))
                                                                                            (rnode-right roster))]
        [else (make-rnode (make-student (student-id (rnode-student roster))
                                        (second (first listof-updates))
                                        (student-grade (rnode-student roster)))
                          (rnode-left roster) (rnode-right roster))]))