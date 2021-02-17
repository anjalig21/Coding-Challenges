;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname groceries) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 05, Problem 2a
;; ***************************************************

(define-struct grocery (dept name cost mass))
;; A Grocery is a (make-grocery Str Str Num Num)
;; Requires: cost >= 0, mass > 0.

;; A Store is a (listof Grocery)
;; Requires: no two items have both the same dept and same name.

;; Stores
(define try-n-save
  (list (make-grocery "produce" "apple" 2.49 600)
        (make-grocery "seed" "rice" 0.95 1000)
        (make-grocery "dairy" "milk" 3.99 4000)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "potato" 2.99 5000)
        (make-grocery "chips" "potato" 1.99 250)
        (make-grocery "chips" "corn" 1.99 275)
        (make-grocery "seed" "wheat" 0.49 500)
        (make-grocery "produce" "banana" 0.69 450)
        (make-grocery "dairy" "cheese" 6.49 900)
        (make-grocery "chips" "banana" 1.99 50)
        (make-grocery "produce" "peach" 3.99 400)
        (make-grocery "seed" "lentil" 2.99 800)
        (make-grocery "produce" "corn" 0.99 100)
        (make-grocery "seed" "corn" 4.99 850)
        (make-grocery "dairy" "kefir" 5.99 1000)))

(define kwik-e-mart
  (list (make-grocery "seed" "rice" 0.38 400)
        (make-grocery "can" "corn" 4.00 400)
        (make-grocery "seed" "pinto" 2.49 500)
        (make-grocery "produce" "apple" 2.99 400)
        (make-grocery "can" "creamed eels" 2.19 350)
        (make-grocery "produce" "pineapple" 3.17 250)))

(define student-shop
  (list (make-grocery "dairy" "milk" 1.50 100)
        (make-grocery "dairy" "cheese" 0.75 25)
        (make-grocery "dairy" "yogurt" 3.50 100)
        (make-grocery "fruits" "apples" 1.50 50)
        (make-grocery "fruits" "oranges" 1.00 100)
        (make-grocery "fruits" "guavas" 2.50 150)
        (make-grocery "nuts" "peanuts" 2.00 50)
        (make-grocery "nuts" "pistachios" 6.00 400)
        (make-grocery "nuts" "walnuts" 4.00 125)
        (make-grocery "can" "corn" 4.00 400)))


;; ***************************************************
;; Problem 2b
;; ***************************************************

(define-struct interval (lo hi))
;; An Interval is a (make-interval (anyof 'dontcare Num) (anyof 'dontcare Num))

;; (in-interval? num interval) consumes a Num, num
;; and an Interval, interval. It produces true if
;; the Num is in the interval, and false otherwise.

;; Examples
(check-expect (in-interval? 42 (make-interval 'dontcare 'dontcare)) true)
(check-expect (in-interval? 34 (make-interval 35 'dontcare)) false)
(check-expect (in-interval? 34 (make-interval 'dontcare 35)) true)

;; in-interval?: Num Interval -> Bool
(define (in-interval? num interval)
  (cond [(and (symbol? (interval-lo interval))
              (symbol? (interval-hi interval))) true]
        [(and (symbol? (interval-lo interval)) (<= num (interval-hi interval))) true]
        [(and (symbol? (interval-lo interval)) (> num (interval-hi interval))) false]
        [(and (symbol? (interval-hi interval)) (>= num (interval-lo interval))) true]
        [(and (symbol? (interval-hi interval)) (< num (interval-lo interval))) false]
        [(and (>= num (interval-lo interval)) (<= num (interval-hi interval))) true]
        [else false]))

;; Tests
(check-expect (in-interval? 34 (make-interval 34 34)) true)
(check-expect (in-interval? 35 (make-interval 34 34)) false)
(check-expect (in-interval? 35 (make-interval 30 34)) false)
(check-expect (in-interval? 35 (make-interval 'dontcare 34)) false)
(check-expect (in-interval? 35 (make-interval 30 'dontcare)) true)


;; ***************************************************
;; Problem 2c
;; ***************************************************

;; A StrPatt is a (anyof Str 'dontcare)

(define-struct query (dept name cost mass))
;; A GroceryQuery is a
;; (make-query StrPatt StrPatt Interval Interval)

;; (find-matches listof-grocery GroceryQuery) consumes a list of Grocery, listof-grocery, and a
;; GroceryQuery. It produces a list containing the items from the list that satisfy the
;; GroceryQuery.

;; Examples
(check-expect (find-matches try-n-save
                            (make-query "seed" 'dontcare
                                        (make-interval 'dontcare 'dontcare)
                                        (make-interval 'dontcare 'dontcare))) (list (make-grocery "seed" "rice" 0.95 1000)
                                                                                    (make-grocery "seed" "pinto" 2.49 500)
                                                                                    (make-grocery "seed" "wheat" 0.49 500)
                                                                                    (make-grocery "seed" "lentil" 2.99 800)
                                                                                    (make-grocery "seed" "corn" 4.99 850)))
(check-expect (find-matches try-n-save
                            (make-query "seed" 'dontcare
                                        (make-interval 'dontcare 3.00)
                                        (make-interval 600 'dontcare))) (list
                                                                         (make-grocery "seed" "rice" 0.95 1000)
                                                                         (make-grocery "seed" "lentil" 2.99 800)))

;; find-matches: (listof Grocery) GroceryQuery -> (listof Grocery)
(define (find-matches listof-grocery GroceryQuery)
  (cond [(empty? listof-grocery) empty]
        [(and (name/dept=? (query-dept GroceryQuery) (grocery-dept (first listof-grocery)))
              (name/dept=? (query-name GroceryQuery) (grocery-name (first listof-grocery)))
              (in-interval? (grocery-cost (first listof-grocery)) (query-cost GroceryQuery))
              (in-interval? (grocery-mass (first listof-grocery)) (query-mass GroceryQuery)))
         (cons (first listof-grocery) (find-matches (rest listof-grocery) GroceryQuery))]
        [else (find-matches (rest listof-grocery) GroceryQuery)]))

;; Tests
(check-expect (find-matches student-shop
                            (make-query 'dontcare 'dontcare
                                        (make-interval 'dontcare 'dontcare)
                                        (make-interval 'dontcare 'dontcare))) student-shop)
(check-expect (find-matches kwik-e-mart
                            (make-query "fruits" 'dontcare
                                        (make-interval 'dontcare 3.00)
                                        (make-interval 600 'dontcare))) empty)
(check-expect (find-matches student-shop
                            (make-query "fruits" 'dontcare
                                        (make-interval 'dontcare 'dontcare)
                                        (make-interval 'dontcare 'dontcare))) (list (make-grocery "fruits" "apples" 1.50 50)
                                                                                    (make-grocery "fruits" "oranges" 1.00 100)
                                                                                    (make-grocery "fruits" "guavas" 2.50 150)))


;; (name/dept=? first-name second-name) consumes either strings,
;; or symbols ('dontcare), first-name and second-name,
;; and checks if they are equal.

;; Examples
(check-expect (name/dept=? "seeds" "seeds") true)
(check-expect (name/dept=? 'dontcare "seeds") true)
(check-expect (name/dept=? "fruits" "seeds") false)

;; name/dept=?: (anyof Str Sym) -> Bool
(define (name/dept=? first-name second-name)
  (cond [(symbol? first-name) true]
        [(string=? first-name second-name) true]
        [else false]))


;; ***************************************************
;; Problem 2d
;; ***************************************************

;; (sort-dept-name store) consumes a Store, store, and produces the same
;; values, sorted alphabetically by department, then alphabetically by name.

;; Examples
(check-expect (sort-dept-name (rest try-n-save))
              (list
               (make-grocery "chips" "banana" 1.99 50)
               (make-grocery "chips" "corn" 1.99 275)
               (make-grocery "chips" "potato" 1.99 250)
               (make-grocery "dairy" "cheese" 6.49 900)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "dairy" "milk" 3.99 4000)
               (make-grocery "produce" "banana" 0.69 450)
               (make-grocery "produce" "corn" 0.99 100)
               (make-grocery "produce" "peach" 3.99 400)
               (make-grocery "produce" "potato" 2.99 5000)
               (make-grocery "seed" "corn" 4.99 850)
               (make-grocery "seed" "lentil" 2.99 800)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.95 1000)
               (make-grocery "seed" "wheat" 0.49 500)))
(check-expect (sort-dept-name try-n-save)
              (list
               (make-grocery "chips" "banana" 1.99 50)
               (make-grocery "chips" "corn" 1.99 275)
               (make-grocery "chips" "potato" 1.99 250)
               (make-grocery "dairy" "cheese" 6.49 900)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "dairy" "milk" 3.99 4000)
               (make-grocery "produce" "apple" 2.49 600)
               (make-grocery "produce" "banana" 0.69 450)
               (make-grocery "produce" "corn" 0.99 100)
               (make-grocery "produce" "peach" 3.99 400)
               (make-grocery "produce" "potato" 2.99 5000)
               (make-grocery "seed" "corn" 4.99 850)
               (make-grocery "seed" "lentil" 2.99 800)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.95 1000)
               (make-grocery "seed" "wheat" 0.49 500)))

;; sort-dept-name: Store -> Store
(define (sort-dept-name store)
  (sort-dept (sort-name store)))

;; Tests
(check-expect (sort-dept-name student-shop)
              (list
               (make-grocery "can" "corn" 4 400)
               (make-grocery "dairy" "cheese" 0.75 25)
               (make-grocery "dairy" "milk" 1.5 100)
               (make-grocery "dairy" "yogurt" 3.5 100)
               (make-grocery "fruits" "apples" 1.5 50)
               (make-grocery "fruits" "guavas" 2.5 150)
               (make-grocery "fruits" "oranges" 1 100)
               (make-grocery "nuts" "peanuts" 2 50)
               (make-grocery "nuts" "pistachios" 6 400)
               (make-grocery "nuts" "walnuts" 4 125)))
(check-expect (sort-dept-name kwik-e-mart)
              (list
               (make-grocery "can" "corn" 4 400)
               (make-grocery "can" "creamed eels" 2.19 350)
               (make-grocery "produce" "apple" 2.99 400)
               (make-grocery "produce" "pineapple" 3.17 250)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.38 400)))


;; (sort-dept store) consumes a Store, store,
;; and sorts its elements by department alphabetically

;; Examples
(check-expect (sort-dept kwik-e-mart)
              (list
               (make-grocery "can" "corn" 4 400)
               (make-grocery "can" "creamed eels" 2.19 350)
               (make-grocery "produce" "apple" 2.99 400)
               (make-grocery "produce" "pineapple" 3.17 250)
               (make-grocery "seed" "rice" 0.38 400)
               (make-grocery "seed" "pinto" 2.49 500)))
(check-expect (sort-dept-name student-shop)
              (list
               (make-grocery "can" "corn" 4 400)
               (make-grocery "dairy" "cheese" 0.75 25)
               (make-grocery "dairy" "milk" 1.5 100)
               (make-grocery "dairy" "yogurt" 3.5 100)
               (make-grocery "fruits" "apples" 1.5 50)
               (make-grocery "fruits" "guavas" 2.5 150)
               (make-grocery "fruits" "oranges" 1 100)
               (make-grocery "nuts" "peanuts" 2 50)
               (make-grocery "nuts" "pistachios" 6 400)
               (make-grocery "nuts" "walnuts" 4 125)))

;; sort-dept: Store -> Store
(define (sort-dept store)
  (cond [(empty? store) empty]
        [else (insert-dept (first store) (sort-dept (rest store)))]))


;; (sort-name store) consumes a Store, store,
;; and sorts its elements by name alphabetically

;; Examples
(check-expect (sort-name kwik-e-mart)
              (list
               (make-grocery "produce" "apple" 2.99 400)
               (make-grocery "can" "corn" 4 400)
               (make-grocery "can" "creamed eels" 2.19 350)
               (make-grocery "produce" "pineapple" 3.17 250)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.38 400)))
(check-expect (sort-name student-shop)
              (list
               (make-grocery "fruits" "apples" 1.5 50)
               (make-grocery "dairy" "cheese" 0.75 25)
               (make-grocery "can" "corn" 4 400)
               (make-grocery "fruits" "guavas" 2.5 150)
               (make-grocery "dairy" "milk" 1.5 100)
               (make-grocery "fruits" "oranges" 1 100)
               (make-grocery "nuts" "peanuts" 2 50)
               (make-grocery "nuts" "pistachios" 6 400)
               (make-grocery "nuts" "walnuts" 4 125)
               (make-grocery "dairy" "yogurt" 3.5 100)))

;; sort-name: Store -> Store
(define (sort-name store)
  (cond [(empty? store) empty]
        [else (insert-name (first store) (sort-name (rest store)))]))


;; (insert-dept value-to-insert listof-values) inserts the items
;; in a grocery list, value-to-insert, into the sorted list listof-values
;; so that the resulting list is also sorted according to department.

;; Examples:
(check-expect (insert-dept (make-grocery "chips" "banana" 1.99 50) try-n-save)
              (list
               (make-grocery "chips" "banana" 1.99 50)
               (make-grocery "chips" "potato" 1.99 250)
               (make-grocery "chips" "corn" 1.99 275)
               (make-grocery "chips" "banana" 1.99 50)
               (make-grocery "dairy" "milk" 3.99 4000)
               (make-grocery "dairy" "cheese" 6.49 900)
               (make-grocery "dairy" "kefir" 5.99 1000)
               (make-grocery "produce" "apple" 2.49 600)
               (make-grocery "produce" "potato" 2.99 5000)
               (make-grocery "produce" "banana" 0.69 450)
               (make-grocery "produce" "peach" 3.99 400)
               (make-grocery "produce" "corn" 0.99 100)
               (make-grocery "seed" "rice" 0.95 1000)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "wheat" 0.49 500)
               (make-grocery "seed" "lentil" 2.99 800)
               (make-grocery "seed" "corn" 4.99 850)))
(check-expect (insert-dept (make-grocery "seed" "corn" 4.99 850) kwik-e-mart)
              (list
               (make-grocery "seed" "corn" 4.99 850)
               (make-grocery "can" "corn" 4 400)
               (make-grocery "can" "creamed eels" 2.19 350)
               (make-grocery "produce" "apple" 2.99 400)
               (make-grocery "produce" "pineapple" 3.17 250)
               (make-grocery "seed" "rice" 0.38 400)
               (make-grocery "seed" "pinto" 2.49 500)))
 
;; insert-dept: Grocery Store -> Store
(define (insert-dept value-to-insert listof-values)
  (cond [(empty? listof-values) (cons value-to-insert empty)]
        [(string<=? (grocery-dept value-to-insert) (grocery-dept (first listof-values)))
         (cons value-to-insert (sort-dept listof-values))]
        [else (cons (first listof-values) (insert-dept value-to-insert (rest listof-values)))]))


;; (insert-name value-to-insert listof-values) inserts the items
;; in a grocery list, value-to-insert, into the sorted list listof-values
;; so that the resulting list is also sorted according to name.

;; Examples:
(check-expect (insert-name (make-grocery "seed" "pinto" 2.49 500) student-shop)
              (list
               (make-grocery "dairy" "milk" 1.5 100)
               (make-grocery "dairy" "cheese" 0.75 25)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "fruits" "apples" 1.5 50)
               (make-grocery "can" "corn" 4 400)
               (make-grocery "fruits" "guavas" 2.5 150)
               (make-grocery "fruits" "oranges" 1 100)
               (make-grocery "nuts" "peanuts" 2 50)
               (make-grocery "nuts" "pistachios" 6 400)
               (make-grocery "nuts" "walnuts" 4 125)
               (make-grocery "dairy" "yogurt" 3.5 100)))
(check-expect (insert-name (make-grocery "seed" "pinto" 2.49 500) kwik-e-mart)
              (list
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "produce" "apple" 2.99 400)
               (make-grocery "can" "corn" 4 400)
               (make-grocery "can" "creamed eels" 2.19 350)
               (make-grocery "produce" "pineapple" 3.17 250)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "seed" "rice" 0.38 400)))
 
;; insert-name: Grocery Store -> Store
(define (insert-name value-to-insert listof-values)
  (cond [(empty? listof-values) (cons value-to-insert empty)]
        [(string<=? (grocery-name value-to-insert) (grocery-name (first listof-values)))
         (cons value-to-insert (sort-name listof-values))]
        [else (cons (first listof-values) (insert-name value-to-insert (rest listof-values)))]))


;; ***************************************************
;; Problem 2e
;; ***************************************************

;; (overlap store1 store2) consumes two Stores, store1 and store2, and produces
;; a Store, containing only those items available in both stores, which is cheaper, per
;; gram. For items with equal cost per gram, the smaller package is preferred.

;; Examples
(check-expect (overlap kwik-e-mart try-n-save) (list
                                                (make-grocery "produce" "apple" 2.49 600)
                                                (make-grocery "seed" "pinto" 2.49 500) 
                                                (make-grocery "seed" "rice" 0.38 400)))
(check-expect (overlap student-shop kwik-e-mart)
              (list (make-grocery "can" "corn" 4 400)))

;; overlap: Store Store -> Store
(define (overlap store1 store2)
  (overlap-recursion (sort-dept-name store1) (sort-dept-name store2) (sort-dept-name store2)))

;; Tests
(check-expect (overlap try-n-save student-shop)
              (list (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "dairy" "milk" 3.99 4000)))
(check-expect (overlap try-n-save (list (make-grocery "seafood" "crabs" 25.00 1000))) empty)


;; (overlap-recursion store1 store2 store2-c) consumes 3 stores,
;; store1, store2, and store2-c and produces
;; a Store, containing only those items available in both stores,
;; which is cheaper, per gram.

;; Examples
(check-expect (overlap-recursion try-n-save student-shop student-shop)
              (list (make-grocery "dairy" "cheese" 6.49 900)
                    (make-grocery "dairy" "milk" 3.99 4000)))
(check-expect (overlap-recursion try-n-save
                                 (list (make-grocery "seafood" "crabs" 25.00 1000))
                                 (list (make-grocery "seafood" "crabs" 25.00 1000))) empty)

;; Store Store Store -> Store
(define (overlap-recursion store1 store2 store2-c)
  (cond [(empty? store1) empty]
        [(empty? store2) (overlap (rest store1) store2-c)]
        [(and (string=? (grocery-name (first store1)) (grocery-name (first store2)))
              (string=? (grocery-dept (first store1)) (grocery-dept (first store2))))
         (cons (cheaper-per-gram (first store1) (first store2)) (overlap-recursion store1 (rest store2) store2-c))]
        [else (overlap-recursion store1 (rest store2) store2-c)]))


;; (cheaper-per-gram grocery1 grocery2) consumes two groceries, grocery1
;; and grocery2, and outputs the cheaper option per gram.
         
;; Examples
(check-expect (cheaper-per-gram (make-grocery "fruits" "apples" 2.50 100)
                                (make-grocery "fruits" "apples" 2.50 100))
              (make-grocery "fruits" "apples" 2.50 100))
(check-expect (cheaper-per-gram (make-grocery "fruits" "apples" 5.00 200)
                                (make-grocery "fruits" "apples" 2.50 100))
              (make-grocery "fruits" "apples" 2.50 100))

;; cheaper-per-gram: Grocery Grocery -> Grocery
(define (cheaper-per-gram grocery1 grocery2)
  (cond [(< (/ (grocery-cost grocery1) (grocery-mass grocery1))
            (/ (grocery-cost grocery2) (grocery-mass grocery2))) grocery1]
        [(and (= (/ (grocery-cost grocery1) (grocery-mass grocery1))
            (/ (grocery-cost grocery2) (grocery-mass grocery2))) (<= (grocery-mass grocery1) (grocery-mass grocery2))) grocery1]
        [else grocery2]))
         

;; ***************************************************
;; Problem 2f
;; ***************************************************

;; (scale-prices store GroceryQuery num) consumes a Store, store, a GroceryQuery,
;; GroceryQuery, and a non-negative Num representing a ratio. All items in
;; the Store that satisfy the GroceryQuery will have their price changed by the ratio,
;; rounded to the nearest 0.01. The order of items remain the same.

;; Examples
(check-expect (scale-prices kwik-e-mart
                            (make-query "can" 'dontcare
                                        (make-interval 'dontcare 'dontcare)
                                        (make-interval 'dontcare 'dontcare)) 1.10)
              (list (make-grocery "seed" "rice" 0.38 400)
                    (make-grocery "can" "corn" 4.40 400) 
                    (make-grocery "seed" "pinto" 2.49 500)
                    (make-grocery "produce" "apple" 2.99 400)
                    (make-grocery "can" "creamed eels" 2.41 350) 
                    (make-grocery "produce" "pineapple" 3.17 250)))
(check-expect (scale-prices kwik-e-mart
                            (make-query 'dontcare 'dontcare
                                        (make-interval 'dontcare 'dontcare)
                                        (make-interval 'dontcare 'dontcare)) 0)
              (list (make-grocery "seed" "rice" 0 400)
                    (make-grocery "can" "corn" 0 400)
                    (make-grocery "seed" "pinto" 0 500)
                    (make-grocery "produce" "apple" 0 400)
                    (make-grocery "can" "creamed eels" 0 350) 
                    (make-grocery "produce" "pineapple" 0 250)))

;; scale-prices: Store GroceryQuery Num -> Store
;;  requires Num >= 0
(define (scale-prices store GroceryQuery num)
  (cond [(empty? store) empty]
        [(and (name/dept=? (query-dept GroceryQuery) (grocery-dept (first store)))
              (name/dept=? (query-name GroceryQuery) (grocery-name (first store)))
              (in-interval? (grocery-cost (first store)) (query-cost GroceryQuery))
              (in-interval? (grocery-mass (first store)) (query-mass GroceryQuery)))
         (cons (make-grocery (grocery-dept (first store))
                             (grocery-name (first store))
                             (round-2d (* (grocery-cost (first store)) num))
                             (grocery-mass (first store)))
               (scale-prices (rest store) GroceryQuery num))]
        [else (cons (first store) (scale-prices (rest store) GroceryQuery num))]))

;; Tests
(check-expect (scale-prices try-n-save (make-query "produce" "apple"
                                        (make-interval 0 3)
                                        (make-interval 600 600)) 2)
              (list
               (make-grocery "produce" "apple" 4.98 600)
               (make-grocery "seed" "rice" 0.95 1000)
               (make-grocery "dairy" "milk" 3.99 4000)
               (make-grocery "seed" "pinto" 2.49 500)
               (make-grocery "produce" "potato" 2.99 5000)
               (make-grocery "chips" "potato" 1.99 250)
               (make-grocery "chips" "corn" 1.99 275)
               (make-grocery "seed" "wheat" 0.49 500)
               (make-grocery "produce" "banana" 0.69 450)
               (make-grocery "dairy" "cheese" 6.49 900)
               (make-grocery "chips" "banana" 1.99 50)
               (make-grocery "produce" "peach" 3.99 400)
               (make-grocery "seed" "lentil" 2.99 800)
               (make-grocery "produce" "corn" 0.99 100)
               (make-grocery "seed" "corn" 4.99 850)
               (make-grocery "dairy" "kefir" 5.99 1000)))
(check-expect (scale-prices student-shop (make-query "fruits" "apples"
                                        (make-interval 0 'dontcare)
                                        (make-interval 'dontcare 600)) 2)
              (list
               (make-grocery "dairy" "milk" 1.5 100)
               (make-grocery "dairy" "cheese" 0.75 25)
               (make-grocery "dairy" "yogurt" 3.5 100)
               (make-grocery "fruits" "apples" 3 50)
               (make-grocery "fruits" "oranges" 1 100)
               (make-grocery "fruits" "guavas" 2.5 150)
               (make-grocery "nuts" "peanuts" 2 50)
               (make-grocery "nuts" "pistachios" 6 400)
               (make-grocery "nuts" "walnuts" 4 125)
               (make-grocery "can" "corn" 4 400)))
(check-expect (scale-prices student-shop (make-query 'dontcare 'dontcare
                                        (make-interval 'dontcare 'dontcare)
                                        (make-interval 'dontcare 'dontcare)) 1) student-shop)

;; (round-2d num) consumes a number, num, and outputs
;; the num rounded to 2 decimal places.

;; Examples
(check-expect (round-2d 100) 100)
(check-expect (round-2d 2.44455566) 2.44)
 
;; round-2d: Num -> Num
(define (round-2d num)
  (/ (round (* num 100)) 100))