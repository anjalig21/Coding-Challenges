;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Starter Code
;; ***************************************************
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       


;; ****************************************************************************
;; 3ai)
;; These are helper functions. See assignment for design recipe requirements.
;; ****************************************************************************

;; (get-x point) consumes a Point and produces, respectively,
;; the x-coordinate of the point.

;; Examples
(check-expect (get-x (list 5 4)) 5)
(check-expect (get-x (list 0 4)) 0)
(check-expect (get-x empty) empty)

;; get-x: Point -> Num
(define (get-x point)
  (cond [(empty? point) empty]
        [else (first point)]))


;; (get-y point) consumes a Point and produces, respectively,
;; the y-coordinate of the point.

;; Examples
(check-expect (get-y (list 5 4)) 4)
(check-expect (get-y (list 4 0)) 0)
(check-expect (get-y empty) empty)

;; get-y: Point -> Num
(define (get-y point)
  (cond [(empty? point) empty]
        [else (first (rest point))]))


;; ****************************************************************************
;; 3aii)
;; ****************************************************************************

;; (translate-gesture gesture x-offset y-offset) consumes a Gesture, gesture,
;; and two numbers, an x-offset and a y-offset, and produces a new gesture
;; such that each Point ((list x y)) in the original gesture now has value
;; (list (+ x x-offset) (+ y y-offset)) in the new gesture.

;; Examples
(check-expect (translate-gesture (list (list 5 6) (list 5 7) (list 5 8)) 1 2)
              (list (list 6 8) (list 6 9) (list 6 10)))
(check-expect (translate-gesture (list (list 5 5) (list 5 5) (list 0 0)) 0 0)
              (list (list 5 5) (list 5 5) (list 0 0)))

;; translate-gesture: Gesture Num Num -> Gesture
(define (translate-gesture gesture x-offset y-offset)
  (cond [(empty? gesture) empty]
        [else (cons (cons (+ (get-x (first gesture)) x-offset)
                          (cons (+ (get-y (first gesture)) y-offset) empty))
                    (translate-gesture (rest gesture) x-offset y-offset))]))


;; ****************************************************************************
;; 3aiii)
;; ****************************************************************************

;; (scale-gesture gesture x-scale y-scale) consumes a Gesture, gesture,
;; and two numbers, x-scale and y-scale, and produces a new stroke such that
;; each Point ((list x y)) in the original gesture now has value
;; (list (* x x-scale) (* y y-scale)) in the new gesture.

;; Examples
(check-expect (scale-gesture (list (list 5 6) (list 5 7) (list 5 8)) 1 2)
              (list (list 5 12) (list 5 14) (list 5 16)))
(check-expect (scale-gesture (list (list 5 6) (list 5 7) (list 5 8)) 0 0)
              (list (list 0 0) (list 0 0) (list 0 0)))

;; scale-gesture: Gesture Num Num -> Gesture
;;  requires x-scale and y-scale to be positive, non-zero values.
(define (scale-gesture gesture x-scale y-scale)
  (cond [(empty? gesture) empty]
        [else (cons (cons (* (get-x (first gesture)) x-scale)
                          (cons (* (get-y (first gesture)) y-scale) empty))
                    (scale-gesture (rest gesture) x-scale y-scale))]))


;; ****************************************************************************
;; 3aiv)
;; ****************************************************************************

;; (get-b-box gesture) consumes a non-empty Gesture, gesture,
;; and produces the gesture’s BoundingBox.

;; Examples
(check-expect (get-b-box (list (list 100 0) (list 200 100) (list 100 200) (list 0 100) (list 100 0)))
              (list (list 0 0) (list 200 200)))
(check-expect (get-b-box (list ( list 100 100)))
              (list (list 100 100) (list 100 100)))

;; get-b-box: Gesture -> (list Point Point)
;;   requires: gesture to be non-empty
;;   requires: the coordinate values in the first point to be less
;;             than or equal to the respective values in the second point. 
(define (get-b-box gesture)
  (cons (list (min-gesturex gesture) (min-gesturey gesture))
        (cons (list (max-gesturex gesture) (max-gesturey gesture)) empty)))


;; (min-gesturex gesture) consumes a non-empty Gesture, gesture,
;; and outputs the minimum of all the x-values in the points

;; Examples
(check-expect (min-gesturex (list (list 1 2) (list 0 9) (list 5 4))) 0)
(check-expect (min-gesturex (list (list 1 2) (list 5 9) (list 0 4))) 0)
(check-expect (min-gesturex (list (list 0 2) (list 5 9) (list 1 4))) 0)

;; min-gesturex: Gesture -> Num
(define (min-gesturex gesture)
  (cond [(empty? (rest gesture)) (get-x(first gesture))]
        [else (min (get-x (first gesture)) (min-gesturex (rest gesture)))]))


;; (min-gesturey gesture) consumes a non-empty Gesture, gesture,
;; and outputs the minimum of all the y-values in the points

;; Examples
(check-expect (min-gesturey (list (list 1 2) (list 0 9) (list 5 4))) 2)
(check-expect (min-gesturey (list (list 1 2) (list 5 1) (list 0 4))) 1)
(check-expect (min-gesturey (list (list 0 2) (list 5 9) (list 1 0))) 0)

;; min-gesturex: Gesture -> Num
(define (min-gesturey gesture)
  (cond [(empty? (rest gesture)) (get-y (first gesture))]
        [else (min (get-y (first gesture)) (min-gesturey (rest gesture)))]))


;; (max-gesturey gesture) consumes a non-empty Gesture, gesture,
;; and outputs the maximum of all the y-values in the points

;; Examples
(check-expect (max-gesturey (list (list 1 2) (list 0 9) (list 5 4))) 9)
(check-expect (max-gesturey (list (list 1 2) (list 5 1) (list 0 4))) 4)
(check-expect (max-gesturey (list (list 0 10) (list 5 9) (list 1 0))) 10)

;; max-gesturey: Gesture -> Num
(define (max-gesturey gesture)
  (cond [(empty? (rest gesture)) (get-y (first gesture))]
        [else (max (get-y (first gesture)) (max-gesturey (rest gesture)))]))


;; (max-gesturex gesture) consumes a non-empty Gesture, gesture,
;; and outputs the maximum of all the x-values in the points

;; Examples
(check-expect (max-gesturex (list (list 1 2) (list 0 9) (list 5 4))) 5)
(check-expect (max-gesturex (list (list 1 2) (list 5 1) (list 0 4))) 5)
(check-expect (max-gesturex (list (list 10 10) (list 5 9) (list 1 0))) 10)

;; max-gesturex: Gesture -> Num
(define (max-gesturex gesture)
  (cond [(empty? (rest gesture)) (get-x (first gesture))]
        [else (max (get-x (first gesture)) (max-gesturex (rest gesture)))]))


;; ****************************************************************************
;; 3bi)
;; Full design recipe required.
;; ****************************************************************************

;; (gesture-length gesture) consumes a Gesture, gesture,
;; and produces its length. 

;; Examples
(check-within (gesture-length (list (list 1 2) (list 0 9) (list 5 4))) 14.14 0.01)
(check-within (gesture-length (list (list 0 0))) 0 0.01)
(check-expect (gesture-length empty) empty)

;; gesture-length: Gesture -> Num
(define (gesture-length gesture)
  (cond [(empty? gesture) empty]
        [(or (= (length gesture) 0) (= (length gesture) 1)) 0]
        [else (+ (distance-calc (list (- (get-x (second gesture)) (get-x (first gesture)))
                                       (- (get-y (second gesture)) (get-y (first gesture)))))
                 (gesture-length (rest gesture)))]))

;; Tests
(check-within (gesture-length (list (list -5 0) (list 0 0) (list -7 0))) 12 0.01)
(check-within (gesture-length (list (list 0 0) (list 0 0) (list 0 0))) 0 0.01)


;; (distance-calc listof-num) consumes a list of numbers,
;; listof-num, and produces the distance between the adjacent points.

;; Examples
(check-within (distance-calc (list 3 4)) 5 0.01)
(check-within (distance-calc (list 1 2)) 2.24 0.01)

;; distance-calc: (listof Num) -> Num
(define (distance-calc listof-num)
  (sqrt (sumof-squares listof-num)))

;; Tests
(check-within (distance-calc (list 1.4 2 2.8 0)) 3.71 0.01)
(check-within (distance-calc (list -1.4 2 -2.8 0)) 3.71 0.01)
(check-within (distance-calc (list 0 2 0 0)) 2 0.01)


;; (sumof-squares listof-num) consumes a list of numbers, listof-num,
;; and outputs the sum of each number squared in the list.

;; Examples
(check-expect (sumof-squares (list 1 2)) 5)
(check-expect (sumof-squares (list 0 2)) 4)
(check-expect (sumof-squares (list 0 0)) 0)
(check-expect (sumof-squares (list 1.2 2.5)) 7.69)

;; sumof-squares: (listof Num) -> Num
(define (sumof-squares listof-num)
  (cond [(empty? listof-num) 0]
        [else (+ (sqr (first listof-num)) (sumof-squares (rest listof-num)))]))


;; ****************************************************************************
;; 3bii)
;; Full design recipe required.
;; ****************************************************************************

;; Lists
(define mygest (list (list 100 0) (list 200 100)
                     (list 100 200) (list 0 100) (list 100 50)))

;; (get-points g listof-nat) consumes a Gesture, g, and a non-decreasing list
;; of Nat, listof-nat, and produces a Gesture where each Point in the produced
;; Gesture is indexed by one element of the list of Nat consumed.

;; Examples
(check-expect (get-points mygest (list 2 0 2 4 4))
              (list (list 100 200) (list 100 0) (list 100 200) (list 100 50) (list 100 50)))
(check-expect (get-points empty (list 2 0 2 4 4)) empty)

;; get-points: Gesture (listof Nat) -> Gesture
;;   requires: gesture to be non-empty
;;   requires: listof-nat to contain numbers that don't exceed length of gesture
(define (get-points g listof-nat)
  (cond [(empty? g) empty]
        [else (evaluate-points g g listof-nat 0)]))

;; Tests
(check-expect (get-points mygest (list 0 0 0))
              (list (list 100 0) (list 100 0) (list 100 0)))
(check-expect (get-points mygest (list 4 4 4))
              (list (list 100 50) (list 100 50) (list 100 50)))
(check-expect (get-points mygest (list 4 3 2 1 0))
              (list (list 100 50) (list 0 100) (list 100 200) (list 200 100) (list 100 0)))
(check-expect (get-points mygest (list 0 1 2 3 4)) mygest)


;; (evaluate-points g-constant g listof-nat n) consumes two
;; gestures, g-constant and g, a list of natural numbers,
;; listof-nat, and a natural number, n, and then produces
;; a Gesture where each Point in the produced Gesture is
;; indexed by one element of the list of Nat consumed.

;; Examples
(check-expect (evaluate-points mygest mygest (list 2 0 2 4 4) 0)
              (list (list 100 200) (list 100 0) (list 100 200) (list 100 50) (list 100 50)))
(check-expect (evaluate-points mygest mygest (list 2 0) 0)
              (list (list 100 200) (list 100 0)))

;; evaluate-points: Gesture Gesture (listof Nat) Nat -> Gesture
(define (evaluate-points g-constant g listof-nat n)
  (cond [(or (empty? g) (empty? listof-nat)) empty]
        [(= (first listof-nat) n) (cons (first g) (get-points g-constant (rest listof-nat)))]
        [else (evaluate-points g-constant (rest g) listof-nat (add1 n))]))

  
;; ****************************************************************************
;; 3ci)
;; ****************************************************************************

;; Point Positions
(define quarter 0.25)
(define half 0.5)
(define three-quarters 0.75)

;; (five-sample gesture) consumes a Gesture, gesture,
;; and produces a sampling of gesture 5 points:
;; the first, n/4th, n/2th, 3n/4th, and last point.

;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; five-sample: Gesture -> Gesture
;;  requires: gesture is non-empty
(define (five-sample gesture)
  (cond [(empty? gesture) empty]
        [else (get-points gesture (list 0
                            (floor (* (length gesture) quarter))
                            (floor (* (length gesture) half))
                            (floor (* (length gesture) three-quarters))
                            (- (length gesture) 1)))]))

;; Tests:
(check-expect (five-sample empty) empty)
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))


;; ****************************************************************************
;; 3cii)
;; ****************************************************************************

;; (move-and-scale gesture x-scale y-scale) consumes a Gesture, gesture
;; a number, x-scale, and a number, y-scale, and then
;; moves gesture to (0,0) and scales it by (x-scale)x(y-scale)

;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;;  requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture x-scale y-scale)
  (cond [(empty? gesture) empty]
        [else (scale-gesture (translated-gesture gesture) x-scale y-scale)]))

;; Tests:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))
(check-expect (move-and-scale empty 3 0.5) empty)


;; (translate-x gesture) consumes a Gesture, gesture, and
;; and produces the x-offset to translate the gesture to the origin.

;; Examples
(check-expect (translate-x (list (list 1 5) (list 3 4))) -1)
(check-expect (translate-x (list (list 2 5) (list 3 4))) -2)
(check-expect (translate-x (list (list 5 5) (list 3 4))) -3)

;; translate-x: Gesture -> Num
(define (translate-x gesture)
  (- 0 (first (first (get-b-box gesture)))))


;; (translate-y gesture) consumes a Gesture, gesture, and
;; and produces the y-offset to translate the gesture to the origin.

;; Examples
(check-expect (translate-y (list (list 1 5) (list 3 4))) -4)
(check-expect (translate-y (list (list 1 1) (list 3 4))) -1)
(check-expect (translate-y (list (list 0 0) (list 3 4))) 0)

;; translate-y: Gesture -> Num
(define (translate-y gesture)
  (- 0 (second (first (get-b-box gesture)))))


;; (translated-gesture gesture) consumes a Gesture, gesture,
;; and produces the gesture translated to the origin.

;; Examples
(check-expect (translated-gesture (list (list 1 5) (list 3 4)))
              (list (list 0 1) (list 2 0)))
(check-expect (translated-gesture (list (list 0 0) (list 3 4)))
              (list (list 0 0) (list 3 4)))

;; translated-gesture: Gesture -> Gesture
(define (translated-gesture gesture)
  (translate-gesture gesture (translate-x gesture) (translate-y gesture)))


;; ****************************************************************************
;; 3ciii)
;; ****************************************************************************

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;; (normalize-gesture gesture) consumes a Gesture, gesture,
;; and normalizes gesture to (0,0) and a standard size

;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80)))
              (list (list 0 0) (list 57.142857 57.142857) (list 114.285714 114.285714) (list 171.428571 171.428571) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture gesture)
  (cond [(< (calc-width gesture) min-width) (move-and-scale gesture 1 (scale-value-y gesture))]  
        [(< (calc-height gesture) min-height) (move-and-scale gesture (scale-value-x gesture) 1)]
        [else (move-and-scale gesture (scale-value-x gesture) (scale-value-y gesture))]))

;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)


;; (scale-value-y gesture) consumes a Gesture, gesture,
;; and produces the y-scale value to normalize the gesture.

;; Examples
(check-expect (scale-value-y (list (list 0 0) (list 100 50))) 4)
(check-expect (scale-value-y (list (list 0 0) (list 0 50))) 4)
(check-expect (scale-value-y (list (list 0 0) (list 0 200))) 1)

;; scale-value-y: Gesture -> Num
(define (scale-value-y gesture)
  (/ norm-size (get-y (second (get-b-box (translated-gesture gesture))))))


;; (scale-value-x gesture) consumes a Gesture, gesture,
;; and produces the y-scale value to normalize the gesture.

;; Examples
(check-expect (scale-value-x (list (list 0 0) (list 100 50))) 2)
(check-expect (scale-value-x (list (list 0 0) (list 1 50))) 200)
(check-expect (scale-value-x (list (list 0 0) (list 10 50))) 20)

;; scale-value-x: Gesture -> Num
(define (scale-value-x gesture)
  (/ norm-size (get-x (second (get-b-box (translated-gesture gesture))))))


;; (calc-width gesture) consumes a Gesture, gesture,
;; and calculates its width

;; Examples
(check-expect (calc-width (list (list 173 80) (list 173 80) (list 173 81) (list 173 85))) 0)
(check-expect (calc-width (list (list 0 0) (list 173 0) (list 173 81) (list 173 85))) 173)

;; calc-width: Gesture -> Num
(define (calc-width gesture)
  (- (get-x (second (get-b-box gesture))) (get-x (first (get-b-box gesture)))))


;; (calc-height gesture) consumes a Gesture, gesture,
;; and calculates its height

;; Examples
(check-expect (calc-height (list (list 173 80) (list 174 80) (list 175 80))) 0)
(check-expect (calc-height (list (list 2 0) (list 174 80) (list 175 80))) 80)

;; calc-height: Gesture -> Num
(define (calc-height gesture)
  (- (get-y (second (get-b-box gesture))) (get-y (first (get-b-box gesture)))))


;; ****************************************************************************
;; 3civ)
;; ****************************************************************************

;; (geometric-5match gesture1 gesture2) consumes two Gestures, gesture1,
;; and gesture2, and then produces the average distance between
;; points in sub-sampled gesture1 and gesture2 after five-sampling them with 5 points

;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match gesture1 gesture2)
  (cond [(or (empty? gesture1) (empty? gesture2)) empty]
        [else (/ (add-distances (normalized-five-sampled gesture1) (normalized-five-sampled gesture2)) 5)]))

;; Tests:
(check-within (geometric-5match (second (fourth templates))
                                (second (fourth templates))) 0 0.1)
(check-expect (geometric-5match (second (fourth templates)) empty) empty)
(check-expect (geometric-5match empty (second (fourth templates))) empty)


;; (add-distances gesture1 gesture2) consumes two Gestures,
;; gesture1 and gesture2, and produces the distance between
;; the points on the two gestures.

;; Examples
(check-within (add-distances (list (list 5 4) (list 6 7))
                             (list (list 0 0) (list 6 8))) 7.40 0.01)
(check-within (add-distances (list (list 0 0) (list 0 0))
                             (list (list 0 0) (list 6 8))) 10 0.01)

;; add-distances: Gesture Gesture -> Num
(define (add-distances gesture1 gesture2)
  (cond [(empty? gesture1) 0]
        [else (+ (gesture-length (list (first gesture1) (first gesture2)))
                 (add-distances (rest gesture1) (rest gesture2)))]))
  

;; (normalized-five-sampled gesture) consumes a Gesture, gesture,
;; and produces a new gesture that is five-sampled and normalized.

;; Examples
(check-expect (normalized-five-sampled (list (list 0 0) (list 5 5) (list 7 8) (list 5 5)))
              (list (list 0 0) (list 5 125) (list 7 200) (list 5 125) (list 5 125)))
(check-expect (normalized-five-sampled (list (list 0 0) (list 0 0) (list 7 10) (list 0 0)))
              (list (list 0 0) (list 0 0) (list 7 200) (list 0 0) (list 0 0)))

;; normalized-five-sampled: Gesture -> Gesture
(define (normalized-five-sampled gesture)
  (normalize-gesture (five-sample gesture)))


;; ****************************************************************************
;; 3cv)
;; ****************************************************************************

;; (five-point-rec candidate template-library) produces the symbol in
;; template-library closest to candidate.

;; Examples:
 (check-expect (five-point-rec testd templates) 'd)
 (check-expect (five-point-rec testk templates) 'k)
 (check-expect (five-point-rec empty templates) empty)

;; five-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec candidate template-library)
  (cond [(empty? candidate) empty]
        [(= (geometric-5match candidate (second (first template-library)))
            (closest-geometric-5match candidate template-library)) (first (first template-library))]
        [else (five-point-rec-recursion candidate (rest template-library)
                                        (closest-geometric-5match candidate template-library))]))
  
;; Tests
 (check-expect (five-point-rec tests templates) 's)
 (check-expect (five-point-rec testy templates) 'y)
 (check-expect (five-point-rec (second (third templates)) templates) 'c)
 (check-expect (five-point-rec (second (first templates)) templates) 'a)
  

;; (five-point-rec-recursion candidate template-library min-geometric-5match) consumes
;; a Gesture, candidate, a TL, template-library, and number, min-geometric-5match.
;; It then produces the symbol in template-library closest to candidate.

;; Examples
(check-expect (five-point-rec-recursion (second (first templates)) templates 0) 'a)
(check-expect (five-point-rec-recursion (second (second templates)) templates 0) 'b)

;; five-point-rec-recursion: Gesture TL Num -> Sym
(define (five-point-rec-recursion candidate template-library min-geometric-5match)
   (cond [(= (geometric-5match candidate (second (first template-library))) min-geometric-5match)
          (first (first template-library))]
         [else (five-point-rec-recursion candidate (rest template-library) min-geometric-5match)]))


;; (closest-geometric-5match candidate template-library) consumes
;; a Gesture, candidate and a TL, template-library
;; and produces the smallest geometric-5match.

;; Examples
(check-within (closest-geometric-5match (list (list 0 0) (list 7 7)) templates) 30.35 0.01)
(check-within (closest-geometric-5match (list (list 0 10) (list 0 6)) templates) 106.59 0.01)

;; closest-geometric-5match: Gesture TL -> Num
(define (closest-geometric-5match candidate template-library)
  (cond [(empty? (rest template-library)) (geometric-5match candidate (second (first template-library)))]
        [else (min (geometric-5match candidate (second (first template-library)))
                   (closest-geometric-5match candidate (rest template-library)))]))


;; ****************************************************************************
;; 3d)
;; ****************************************************************************

;; (sub-sample gesture k) consumes a Gesture, gesture,
;; and a natural number, k, and then produces a sampling
;; of gesture k points.

;; Examples
(check-expect (sub-sample (list (list 1 1) (list 2 2)) 5)
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)) 5)
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; sub-sample: Gesture Nat -> Gesture
;;  requires Nat > 2
(define (sub-sample gesture k)
  (cond [(empty? gesture) empty]
        [else (get-points gesture (list-for-sample gesture k 0))]))

;; Tests
(check-expect (sub-sample (list (list 1 1) (list 2 2)) 6)
              (list (list 1 1) (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect  (sub-sample (list (list 1 1) (list 2 2)) 4)
               (list (list 1 1) (list 1 1) (list 2 2) (list 2 2)))
(check-expect (sub-sample empty 5) empty)


;; (list-for-sample gesture k num) consumes a Gesture, gesture,
;; a natural number, k and a number, num. It then produces the list
;; required to sample a gesture.

;; Examples
(check-expect (list-for-sample (list (list 1 1) (list 1 1) (list 2 2) (list 2 2)) 4 0)
              (list 0 1 2 3))
(check-expect (list-for-sample (list (list 1 1) (list 2 2) (list 2 2)) 4 1)
              (list 1 2 2))

;; list-for-sample: Gesture Nat Num -> (listof Nat)
(define (list-for-sample gesture k num)
  (cond [(= num (- k 1)) (cons (- (length gesture) 1) empty)]
        [else (cons (floor (* (length gesture) (* (/ 1 (- k 1)) num)))
                    (list-for-sample gesture k (add1 num)))]))


;; (geometric-match gesture1 gesture2 k) consumes two Gestures,
;; gesture1 and gesture2, and produces the average distance between
;; points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points.

;; Examples
(check-within (geometric-match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 5)
               16.16 0.01)

;; geometric-match: Gesture Gesture Nat -> Num
(define (geometric-match gesture1 gesture2 k)
  (cond [(or (empty? gesture1) (empty? gesture2)) empty]
        [else (/ (add-distances (normalized-sampled gesture1 k) (normalized-sampled gesture2 k)) k)]))

;; Tests
(check-within (geometric-match (list (list 6 5) (list 4 3)) (list (list 0 0) (list 5 5)) 4)
              200.04 0.01)
(check-within (geometric-match (list (list 0 0) (list 5 5)) (list (list 0 0) (list 5 5)) 8)
              0.0 0.01)
(check-expect (geometric-match empty (list (list 0 1) (list 5 4) (list 5 4)) 5) empty)


;; (normalized-sampled gesture) consumes a Gesture, gesture,
;; and produces a new gesture that is sub-sampled with k points
;; and normalized.

;; Examples
(check-expect (normalized-sampled (list (list 0 0) (list 5 5) (list 7 8) (list 5 5)) 5)
              (list (list 0 0) (list 5 125) (list 7 200) (list 5 125) (list 5 125)))
(check-expect (normalized-sampled (list (list 0 0) (list 5 0) (list 7 8) (list 5 5)) 5)
              (list (list 0 0) (list 5 0) (list 7 200) (list 5 125) (list 5 125)))

;; normalized-sampled: Gesture -> Gesture
(define (normalized-sampled gesture k)
  (normalize-gesture (sub-sample gesture k)))


;; (k-point-rec candidate template-library k) consumes a Gesture,
;; candidate, a TL, template-library, and a natural number, k, and
;; produces the symbol in template-library closest to candidate.

;; Examples
(check-expect (k-point-rec testd templates 5) 'd)
(check-expect (k-point-rec testk templates 5) 'k)
(check-expect (k-point-rec empty templates 5) empty)

;; k-point-rec: Gesture TL Nat -> Sym
;;  requires: candidate is not both vertical and horizontal
(define (k-point-rec candidate template-library k)
  (cond [(empty? candidate) empty]
        [(= (geometric-match candidate (second (first template-library)) k)
            (closest-geometric-match candidate template-library k)) (first (first template-library))]
        [else (k-point-rec-recursion candidate (rest template-library) k
                                        (closest-geometric-match candidate template-library k))]))
  
;; Tests
 (check-expect (k-point-rec tests templates 5) 's)
 (check-expect (k-point-rec testy templates 5) 'y)
 (check-expect (k-point-rec (second (third templates)) templates 5) 'c)
 (check-expect (k-point-rec (second (first templates)) templates 5) 'a)
  

;; (k-point-rec-recursion candidate template-library k min-geometric-5match) consumes
;; a Gesture, candidate, a TL, template-library, a natural number, k,
;; and number, min-geometric-5match.
;; It then produces the symbol in template-library closest to candidate.

;; Examples
(check-expect (k-point-rec-recursion (second (first templates)) templates 5 0) 'a)
(check-expect (k-point-rec-recursion (second (second templates)) templates 5 0) 'b)

;; k-point-rec-recursion: Gesture TL Nat Num -> Sym
(define (k-point-rec-recursion candidate template-library k min-geometric-match)
   (cond [(= (geometric-match candidate (second (first template-library)) k) min-geometric-match)
          (first (first template-library))]
         [else (k-point-rec-recursion candidate (rest template-library) k min-geometric-match)]))


;; (closest-geometric-match candidate template-library k) consumes
;; a Gesture, candidate, a TL, template-library, and a natural number,
;; k, and produces the smallest geometric-match.

;; Examples
(check-within (closest-geometric-match (list (list 0 0) (list 7 7)) templates 5) 30.35 0.01)
(check-within (closest-geometric-match (list (list 0 0) (list 7 7)) templates 4) 29.54 0.01)

;; closest-geometric-5match: Gesture TL -> Num
(define (closest-geometric-match candidate template-library k)
  (cond [(empty? (rest template-library)) (geometric-match candidate (second (first template-library)) k)]
        [else (min (geometric-match candidate (second (first template-library)) k)
                   (closest-geometric-match candidate (rest template-library) k))]))
