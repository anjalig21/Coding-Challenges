;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname zombies) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 10
;; ***************************************************

;; A Location is a Nat
;; A Horde is a (listof (list Location Nat))
;; A Town is a (listof (list Location (listof Location)))
;; Requires: Town represents a valid graph as defined in Module 16

;; Constant Definition(s)
(define waterloo '((0 (1 2 3))
                   (1 (2 3))
                   (2 (0 4))
                   (3 (1))
                   (4 (5))
                   (5 (3))))

(define space '((0 (4 5 6))
                (4 (5))
                (5 (6))
                (6 (0))))

;; ***************************************************
;; Problem I
;; ***************************************************

;; (infect town zombies) consumes a town, town, (represented as a graph)
;; and a number of zombies, and then produces a horde with that many
;; zombies at each location.

;; Examples
(check-expect (infect waterloo 1000)
              (list (list 0 1000) (list 1 1000) (list 2 1000)
                    (list 3 1000) (list 4 1000) (list 5 1000)))
(check-expect (infect empty 10) empty)

;; infect: Town Nat -> Horde
(define (infect town zombies)
  (foldr (lambda (first-town rest-town)
           (cons (list (first first-town) zombies) rest-town))
         empty town))

;; Tests
(check-expect (infect waterloo 0)
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))
(check-expect (infect space 10)
              (list (list 0 10) (list 4 10)
                    (list 5 10) (list 6 10)))

;; ***************************************************
;; Problem II
;; ***************************************************

;; (sink horde) consumes a Horde, horde, and produces a list
;; of two elements. The first element is the total number
;; of zombies that sink into the earth; the second element is
;; the horde after those zombies have sunken into the earth.

;; Examples
(check-expect (sink (infect waterloo 1000))
              (list 300 (list (list 0 950) (list 1 950) (list 2 950)
                              (list 3 950) (list 4 950) (list 5 950))))
(check-expect (sink (infect empty 1000))
              (list 0 empty))

;; sink: Horde -> (listof Nat Horde)
(define (sink horde)
  (cond [(empty? horde) (list 0 empty)]
        [else (cons (foldr (lambda (first-horde rest-horde)
                             (+ (round (* 0.05 (second first-horde))) rest-horde)) 0 horde)
                    (list (foldr (lambda (first-horde rest-horde)
                                   (cons (list (first first-horde)
                                               (- (second first-horde) (round (* 0.05 (second first-horde)))))
                                         rest-horde)) empty horde)))]))

;; Tests
(check-expect (sink (infect space 10))
              (list 0 (list (list 0 10) (list 4 10)
                            (list 5 10) (list 6 10))))
(check-expect (sink (infect space 11))
              (list 4 (list (list 0 10) (list 4 10)
                            (list 5 10) (list 6 10))))

;; ***************************************************
;; Problem III
;; ***************************************************

;; (apportion zombies n) consumes a natural number representing
;; the number of of zombies, zombies and a positive number, n ways
;; those zombies should be divided. It produces a list of exactly n
;; natural numbers. These numbers must add up to the number of zombies
;; and the difference between any two numbers can’t be greater than 1.

;; Examples
(check-expect (apportion 100 3) (list 34 33 33))
(check-expect (apportion 1 3) (list 1 0 0))
(check-expect (apportion 50 2) (list 25 25))

;; apportion: Nat Nat -> (listof Nat)
;;  requires the second Nat to be positive
(define (apportion zombies n)
  (cond [(= n 1) (cons zombies empty)]
        [else (cons (ceiling (/ zombies n))
                    (apportion (- zombies (ceiling (/ zombies n))) (sub1 n)))]))

;; Tests
(check-expect (apportion 101 3) (list 34 34 33))
(check-expect (apportion 100 4) (list 25 25 25 25))
(check-expect (apportion 101 4) (list 26 25 25 25))
(check-expect (apportion 102 4) (list 26 26 25 25))
(check-expect (apportion 10000 1) (list 10000))

;; ***************************************************
;; Problem IV
;; ***************************************************

;; Constant Definitions
(define braaaaains (second (sink (infect waterloo 1000))))

;; (shamble town horde) consumes a graph representing a town, town and a
;; horde of zombies, horde infesting that town. The function produces the
;; horde that results from all the zombies at each location apportioning
;; themselves into nearly equal groups and shambling along the edges
;; connecting the locations. It is assumed that all locations have
;; outgoing edges and zombies shamble along a single edge each night.

;; Examples
(check-expect (shamble empty empty) empty)
(check-expect (shamble waterloo braaaaains)
              (list (list 0 475) (list 1 1267) (list 2 792)
                    (list 3 1741) (list 4 475) (list 5 950)))
(check-expect (shamble waterloo (infect waterloo 620))
              (list (list 0 310) (list 1 827) (list 2 517)
                    (list 3 1136) (list 4 310) (list 5 620)))

;; shamble: Town Horde -> Horde
(define (shamble town horde)
  (local [;; (apportion-zombies-town town horde) consumes a Town, town
          ;; and a Horde, horde and produces the list of apportions
          ;; according to the number of out-neighbors of a specific Location.
          ;; apportion: Town Horde -> (listof Nat)
          (define (apportion-zombies-town town horde)
            (local [;; (look-for-zombies location horde) consumes a natural number,
                    ;; location and a Horde, horde and produces the number of zombies
                    ;; next to the location in the horde.
                    ;; look-for-zombies: Nat Horde -> Nat
                    (define (look-for-zombies location horde)
                      (foldr (lambda (first-horde rest-horde)
                               (cond [(= (first first-horde) location) (second first-horde)]
                                     [else rest-horde])) empty horde))] 
            (apportion (look-for-zombies (first (first town)) horde)
                       (length (second (first town))))))
          ;; (shamble-duplicates town horde) consumes a Town, town and a Horde, horde
          ;; and produces a horde where there duplicate locations of the zombies
          ;; apportion.
          ;; shamble-duplicates: Town Horde -> Horde
          (define (shamble-duplicates town horde)
            (local [;; (make-horde-lists out-neighbors listof-apportions) consumes two lists of natural
                    ;; numbers, out-neighbors, and listof-appropriates, and produces the seperate
                    ;; hordles according to each location in the town.
                    ;; make-hordle-lists: (listof Nat) (listof Nat) -> Hordle
                    (define (make-horde-lists out-neighbors listof-apportions)
                      (cond [(empty? listof-apportions) empty]
                            [else (cons (list (first out-neighbors) (first listof-apportions))
                                        (make-horde-lists (rest out-neighbors) (rest listof-apportions)))]))]
            (cond [(empty? town) empty]
                  [else (append (make-horde-lists (second (first town)) (apportion-zombies-town town horde))
                                (shamble-duplicates (rest town) horde))])))
          ;; (combine-horde town hordle) consumes a Town, town and a Hordle, hordle, and
          ;; produces the hordle without the duplicates by adding the number of zombies
          ;; in each location.
          ;; combine-hordle: Town Hordle -> Hordle
          (define (combine-horde town hordle)
            (local [;; (add-duplicates location hordle) consumes a natural number, location, and a Hordle, hordle,
                    ;; and produces the number of zombies altogether in that location.
                    ;; add-duplicates: Nat Hordle -> Nat
                    (define (add-duplicates location hordle)
                      (cond [(empty? hordle) 0]
                            [(= location (first (first hordle))) (+ (second (first hordle)) (add-duplicates location (rest hordle)))]
                            [else (add-duplicates location (rest hordle))]))]
              (cond [(empty? town) empty]
                    [else (cons (list (first (first town)) (add-duplicates (first (first town)) hordle))
                                (combine-horde (rest town) hordle))])))]
    (cond [(empty? town) empty]
          [else (combine-horde town (shamble-duplicates town horde))])))

;; Tests
(check-expect (shamble waterloo (infect waterloo 1000))
              (list (list 0 500) (list 1 1334) (list 2 833)
                    (list 3 1833) (list 4 500) (list 5 1000)))
(check-expect (shamble space (infect space 5))
              (list (list 0 5) (list 4 2) (list 5 7) (list 6 6)))
(check-expect (shamble waterloo (infect waterloo 1))
              (list (list 0 1) (list 1 2) (list 2 1)
                    (list 3 1) (list 4 0) (list 5 1)))
(check-expect (shamble (list (list 0 (list 2 1)) (list 1 (list 2 0)) (list 2 (list 1 0)))
                       (infect (list (list 0 (list 2 1)) (list 1 (list 2 0)) (list 2 (list 1 0))) 100))
              (list (list 0 100) (list 1 100) (list 2 100)))

;; ***************************************************
;; Problem V
;; ***************************************************

;; Constant Definitions
(define braaaaaaains (shamble waterloo braaaaains))

;; (rise zombies horde) consumes a natural number of zombies, zombies
;; and a Horde, horde. It produces a new horde with those zombies added
;; to the horde, apportioned as equally as possible between the locations.

;; Examples
(check-expect (rise 300 braaaaaaains)
              (list (list 0 525) (list 1 1317) (list 2 842)
                    (list 3 1791) (list 4 525) (list 5 1000)))
(check-expect (rise 2000 empty) empty)
(check-expect (rise 0 braaaaaaains) braaaaaaains)

;; rise: Nat Horde -> Horde
(define (rise zombies horde)
  (local [;; (increment-zombies horde listof-apportions) consumes a Horde, horde
          ;; a list of natural numbers, listof-apportions, and increases the
          ;; number of zombies in a location by a value in the listof-apportions.
          ;; increment-zombies: Horde (listof Nat) -> Horde
          (define (increment-zombies horde listof-apportions)
            (cond [(empty? horde) empty]
                  [else (cons (list (first (first horde)) (+ (first listof-apportions) (second (first horde))))
                              (increment-zombies (rest horde) (rest listof-apportions)))]))]
  (cond [(empty? horde) empty]
        [else (increment-zombies horde (apportion zombies (length horde)))])))

;; Tests
(check-expect (rise 10 braaaaaaains)
              (list (list 0 477) (list 1 1269) (list 2 794)
                    (list 3 1743) (list 4 476) (list 5 951)))
(check-expect (rise 1 braaaaaaains)
              (list (list 0 476) (list 1 1267) (list 2 792)
                    (list 3 1741) (list 4 475) (list 5 950)))
(check-expect (rise 2 braaaaaaains)
              (list (list 0 476) (list 1 1268) (list 2 792)
                    (list 3 1741) (list 4 475) (list 5 950)))

;; ***************************************************
;; Problem VI
;; ***************************************************

;; (night town horde) consumes a Town, town and a Horde, horde. It
;; produces a new horde after the horrors of a single night have passed.
;; (Horrors: 5% of the zombies at each location sink into the earth, the
;; remaining zombies shamble to a new location, and the sunken zombies rise,
;; apportioned as equally as possible between the locations).

;; Examples
(check-expect (night waterloo (infect waterloo 0))
              (list (list 0 0) (list 1 0) (list 2 0)
                    (list 3 0) (list 4 0) (list 5 0)))
(check-expect (night waterloo (infect waterloo 1000))
              (list (list 0 525) (list 1 1317) (list 2 842)
                    (list 3 1791) (list 4 525) (list 5 1000)))
              
;; night: Town Horde -> Horde
(define (night town horde)
  (rise (first (sink horde))
        (shamble town (second (sink horde)))))

;; Tests
(check-expect (night space (infect space 50))
              (list (list 0 50) (list 4 18)
                    (list 5 66) (list 6 66)))
(check-expect (night space (infect space 0))
              (list (list 0 0) (list 4 0)
                    (list 5 0) (list 6 0)))
(check-expect (night empty empty) empty)

;; ***************************************************
;; Problem VII
;; ***************************************************

;; (apocalypse town infection nights) consumes a Town, town, two natural numbers,
;; one representing the initial number of zombies infecting each location, and a
;; number of nights. It then produces the horde after that many nights have passed.

;; Examples
(check-expect (apocalypse waterloo 1000 3)
              (list (list 0 450) (list 1 1894) (list 2 1104)
                    (list 3 1625)(list 4 450)  (list 5 477)))
(check-expect (apocalypse waterloo 1000 7)
              (list (list 0 544) (list 1 1747) (list 2 1016) 
                    (list 3 1576) (list 4 543) (list 5 574)))

;; apocalypse: Town Nat Nat -> Horde
(define (apocalypse town infection nights)
  (local [;; (horde-nights nights infected-town) consumes a natural number,
          ;; nights and a Horde, infected town, and produces a new Horde
          ;; after that many nights passed.
          ;; horde-nights: Nat Horde -> Horde
          (define (horde-nights nights infected-town)
            (cond [(= 0 nights) infected-town]
                  [else (horde-nights (sub1 nights) (night town infected-town))]))]
    (horde-nights nights (infect town infection))))

;; Tests
(check-expect (apocalypse empty 1000 0) empty)
(check-expect (apocalypse waterloo 1000 0) (infect waterloo 1000))
(check-expect (apocalypse waterloo 1000 14)
              (list (list 0 545) (list 1 1728) (list 2 1040)
                    (list 3 1576) (list 4 545) (list 5 566)))              
(check-expect (apocalypse waterloo 1000 28)
              (list (list 0 545) (list 1 1723) (list 2 1042)
                    (list 3 1578) (list 4 545) (list 5 567)))
(check-expect (apocalypse waterloo 1000 31)
              (list (list 0 545) (list 1 1723) (list 2 1042)
                    (list 3 1579) (list 4 544) (list 5 567)))