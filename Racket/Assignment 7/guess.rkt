;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 07
;; ***************************************************

(require "animals.rkt")

;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)


;; ***************************************************
;; Problem 2a
;; ***************************************************

;; Constants
(define seen (list
              (list 'squirrel 'small 'angry)
              (list 'goose 'large 'swims 'flies 'angry)
              (list 'goose 'large 'swims 'flies 'angry)
              (list 'crow 'medium 'flies 'angry)))
(define squirrel-list (list
                       (list 'squirrel 'small 'angry)
                       (list 'squirrel 'small 'angry)
                       (list 'squirrel 'small 'angry)
                       (list 'squirrel 'small 'angry)))
(define attributes-test (list
                         (list 'squirrel 'small 'angry)
                         (list 'goose 'large 'swims 'flies 'angry)))

;; (collect-attributes examples) consumes a list of Example,
;; examples, and produces a list of attributes contained in
;; the examples with no duplicates.

;; Examples
(check-expect (collect-attributes seen)
              (list 'small 'large 'swims 'medium 'flies 'angry))
(check-expect (collect-attributes squirrel-list)
              (list 'small 'angry))

;; collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes examples)
  (local [;; (remove-duplicates lst) consumes a list of any, and
          ;; removes any duplicates.
          ;; remove-duplicates (listof Any) -> (listof Any)
          (define (remove-duplicates lst)
                   (cond [(empty? lst) empty]
                         [(member? (first lst) (rest lst)) (remove-duplicates (rest lst))]
                         [else (cons (first lst) (remove-duplicates (rest lst)))]))
          ;; (all-attributes lst) consumes a listof Example, lst, and produces
          ;; a list of attributes contained in the examples with duplicates.
          ;; all-attributes: (listof Example) -> (listof Sym)
     (define (all-attributes lst)
        (cond [(empty? examples) empty]
        [(list? (first examples)) (append (collect-attributes (rest (first examples))) (collect-attributes (rest examples)))]
        [else (cons (first examples) (collect-attributes (rest examples)))]))]
     (remove-duplicates (all-attributes examples))))

;; Tests
(check-expect (collect-attributes attributes-test)
              (list 'small 'large 'swims 'flies 'angry))
(check-expect (collect-attributes empty) empty)


;; ***************************************************
;; Problem 2b
;; ***************************************************

;; (split-examples examples symbol) consumes a list of Example, examples
;; and a Symbol, symbol and splits the list of Example on the
;; given symbol.

;; Examples
(check-expect (split-examples seen 'goose) (list
                                            (list
                                             (list 'goose 'large 'swims 'flies 'angry)
                                             (list 'goose 'large 'swims 'flies 'angry))
                                            (list
                                             (list 'squirrel 'small 'angry)
                                             (list 'crow 'medium 'flies 'angry))))
(check-expect (split-examples seen 'small) (list
                                            (list
                                             (list 'squirrel 'small 'angry))
                                            (list
                                             (list 'goose 'large 'swims 'flies 'angry)
                                             (list 'goose 'large 'swims 'flies 'angry)
                                             (list 'crow 'medium 'flies 'angry))))

;; split-examples: (listof Example) Sym -> (list (listof Example) (listof Example))
(define (split-examples examples symbol)
  (local [;; (symbol-in-list symbol lst) consumes a Symbol, symbol and a
          ;; list of Example, lst, and outputs a list of all the lists with
          ;; that symbol.
          ;; symbol-in-list: Sym (listof Example) -> (listof Example)
          (define (symbol-in-list symbol lst)
            (cond [(empty? lst) empty]
                  [(member? symbol (first lst)) (cons (first lst) (symbol-in-list symbol (rest lst)))]
                  [else (symbol-in-list symbol (rest lst))]))
          ;; (not-symbol-list symbol lst) consumes a Symbol, symbol and a
          ;; list of Example, lst, and outputs a list of all the lists that
          ;; don't have that symbol.
          ;; not-symbol-list: Sym (listof Example) -> (listof Example)
          (define (not-symbol-list symbol lst)
            (cond [(empty? lst) empty]
                  [(member? symbol (first lst)) (not-symbol-list symbol (rest lst))]
                  [else (cons (first lst) (not-symbol-list symbol (rest lst)))]))]
    (cons (symbol-in-list symbol examples) (list (not-symbol-list symbol examples)))))

;; Tests
(check-expect (split-examples seen 'angry)
              (list
               (list
                (list 'squirrel 'small 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'crow 'medium 'flies 'angry))
               '()))

(check-expect (split-examples seen 'nice)
              (list
               '()
               (list
                (list 'squirrel 'small 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'crow 'medium 'flies 'angry))))


;; ***************************************************
;; Problem 2c
;; ***************************************************

;; (histogram examples) consumes a list of Example, examples
;; and produces a list of attribute/count pairs, with each
;; pair indicating how many times that attribute appears in the examples.

;; Examples
(check-expect (histogram seen)
              (list
               (list 'small 1) (list 'large 2) (list 'swims 2)
               (list 'medium 1) (list 'flies 3) (list 'angry 4)))
(check-expect (histogram squirrel-list)
              (list
               (list 'small 4) (list 'angry 4)))

;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local [;; (count-attributes symbol examples) consumes a Symbol, symbol, and
          ;; a list of Example, examples and produces the amount of times that
          ;; symbol appears in the list.
          ;; count-attributes: Sym (listof Example) -> Nat
          (define (count-attributes symbol examples)
            (cond [(empty? examples) 0]
                  [(list? (first examples)) (+ (count-attributes symbol (first examples))
                                               (count-attributes symbol (rest examples)))]
                  [(symbol=? symbol (first examples)) (+ 1 (count-attributes symbol (rest examples)))]
                  [else (count-attributes symbol (rest examples))]))
          ;; (combine-histogram listof-attributes examples) consumes a list of Symbols, listof-attributes,
          ;; and a list of Example, examples and produces the Histogram.
          ;; count-attributes: (listof Sym) (listof Example) -> Histogram
          (define (combine-histogram listof-attributes examples)
            (cond [(empty? listof-attributes) empty]
                  [else (cons (list (first listof-attributes) (count-attributes (first listof-attributes) examples))
                              (combine-histogram (rest listof-attributes) examples))]))]
    (combine-histogram (collect-attributes examples) examples)))

;; Tests
(check-expect (histogram attributes-test)
              (list (list 'small 1) (list 'large 1) (list 'swims 1)
                    (list 'flies 1) (list 'angry 2)))
(check-expect (histogram empty) empty)
          

;; ***************************************************
;; Problem 2d
;; ***************************************************

;; (augment-histogram histogram attributes total) consumes a Histogram,
;; histogram, a list of Symbols, attributes and a natural number for
;; total number of examples, total and augments a histogram.

;; Examples
(check-expect (augment-histogram (list (list 'a 100) (list 'c 50))
                                 (list 'a 'b 'c) 200)
              (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))
(check-expect (augment-histogram empty (list 'x 'y) 10)
              (list (list 'x 0 10) (list 'y 0 10)))

;; augment-histogram: Histogram (listof Sym) Nat -> AH
;;   requires: total = (length (listof Example))
(define (augment-histogram histogram attributes total)
  (cond [(empty? attributes) empty] 
        [(empty? histogram)
         (cons (list (first attributes) 0 total)
               (augment-histogram histogram (rest attributes) total))]
                  
        [(symbol=? (first attributes) (first (first histogram)))
         (cons (list (first attributes)
                     (second (first histogram))
                     (- total (second (first histogram))))
               (augment-histogram (rest histogram) (rest attributes)
                                    total))]
        [else (cons (list (first attributes) 0 total)
                    (augment-histogram histogram (rest attributes)
                                         total))]))

;; Tests
(check-expect (augment-histogram empty (list 'f 'g) 10)
              (list (list 'f 0 10) (list 'g 0 10)))
(check-expect (augment-histogram empty empty 10) empty)
(check-expect (augment-histogram
               (list (list 'hello 30) (list 'bye 20) (list 'cs 100))
               (list 'hello 'bye 'cs 'spcom 'math) 150)
              (list
               (list 'hello 30 120) (list 'bye 20 130)
               (list 'cs 100 50) (list 'spcom 0 150)
               (list 'math 0 150)))
(check-expect (augment-histogram empty (list 'n) 10) (list (list 'n 0 10))) 


;; ***************************************************
;; Problem 2e
;; ***************************************************

;; (entropy positive-counts negative-counts) consumes two
;; elements from augmented histograms, positive-counts
;; and negative-counts, and produces their entropy.

;; Examples
(check-within (entropy (list 'large 126 59) (list 'large 146 669))
              #i0.5663948489858 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361))
             #i0.5825593868115 0.001)

;; entropy: (list Sym Nat Nat) (list Sym Nat Nat) -> Num
(define (entropy positive-counts negative-counts)
  (local [;; (prob n m) consumes two numbers, n and m, and produces
          ;; an estimation of a probability from a pair of counts.
          ;; prob: Num Num -> Num
          ;;   requires n>=0 and m>=0
          (define (prob n m)
          (cond [(> (+ n m) 0) (/ n (+ n m))]
                [(= (+ m n) 0) 0.5]))
          ;; (e-prob p) consumes a number, p and completes an
          ;; estimation of a probability.
          ;; e-prob: Num -> Num
          ;;  requires p >= 0 and p <= 1
          (define (e-prob p)
            (cond [(and (> p 0) (<= p 1)) (* -1 (* p (log p 2)))]
                  [(= p 0) 0]))
          ;; Variable Definitions 
          (define a (second positive-counts))
          (define b (second negative-counts))
          (define c (third positive-counts))
          (define d (third negative-counts))]
    (+ (* (prob (+ a b) (+ c d))
          (+ (e-prob (prob a b)) (e-prob (prob b a))))
       (* (prob (+ c d) (+ a b))
          (+ (e-prob (prob c d)) (e-prob (prob d c)))))))

;; Tests
(check-within (entropy (list 'a 0 100) (list 'b 100 0))
              0.0 0.001)
(check-within (entropy (list 'large 0 59) (list 'large 0 669))
              #i0.40584699467467716 0.001)


;; ***************************************************
;; Problem 2f
;; ***************************************************

;; (entropy-attributes positive negative) consumes two
;; Augmented Histograms (AH), positive and negative, and
;; computes the entropy of each attribute, producing
;; a list of attribute/entropy pairs.

;; Examples
(check-within (entropy-attributes (list
                                   (list 'large 126 59) (list 'angry 161 24)
                                   (list 'small 17 168) (list 'flies 170 15)
                                   (list 'swims 162 23) (list 'medium 42 143))
                                  (list
                                   (list 'large 146 669) (list 'angry 469 346)
                                   (list 'small 454 361) (list 'flies 615 200)
                                   (list 'swims 365 450) (list 'medium 215 600)))
              (list
               (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
               (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
               (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677)) 0.001)

;; entropy-attributes: AH AH -> EAL
(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive)) (entropy (first positive) (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))


;; ***************************************************
;; Problem 2g
;; ***************************************************

;; (best-attribute entropies) consumes a non-empty list of
;; attribute/entropy pairs, entropies, and produces the
;; attribute with the minimum entropy.

;; Examples
(check-expect (best-attribute (list
                               (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
                               (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
                               (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)
(check-expect (best-attribute (list
                               (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
                               (list 'small #i0.5825593868115) (list 'flies #i0.56639484898584)
                               (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)

;; best-attribute: EAL -> Sym
;;  requires EAL to be non-empty
(define (best-attribute entropies)
  (local [;; (min-entropy entropies) consumes non-empty list of
          ;; attribute/entropy pairs, entropies, and produces the
          ;; minimum entropy.
          ;; min-entropy: EAL -> Num
          (define (min-entropy entropies)
            (cond [(empty? (rest entropies)) (second (first entropies))]
                  [else (min (second (first entropies))
                             (min-entropy (rest entropies)))]))
          ;; (best-attribute-calc entropies entropies-constant) consumes two
          ;; non-empty lists of attribute/entropy pairs, entropies and entropies-constant
          ;; and outputs the symbol with the minimum entropy.
          ;; best-attribute-calc: EAL EAL -> Sym
          (define (best-attribute-calc entropies entropies-constant)
            (cond [(= (min-entropy entropies-constant) (second (first entropies))) (first (first entropies))]
                  [else (best-attribute-calc (rest entropies) entropies-constant)]))]
    (best-attribute-calc entropies entropies)))

;; Tests
(check-expect (best-attribute (list
                               (list 'large #i0.9663948489858) (list 'angry #i0.6447688190492)
                               (list 'small #i0.5825593868115) (list 'flies #i0.56639484898584)
                               (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'flies)


;; ***************************************************
;; Problem 2h
;; ***************************************************

;; (build-dt examples label) consumes a list of Examples,
;; and Symbol, label and then produces a Decision Tree
;; for that label.

;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
  (local [;; (attributes examples) consumes a list of Example, examples,
          ;; and produces the list of attributes.
          ;; attributes: (listof Example) -> (listof Sym)
          (define attributes (collect-attributes examples))
          ;; (split examples label) consumes a list of Example, examples,
          ;; and a Symbol, label an produces a list of Example with the symbol
          ;; and a list of Example without the symbol.
          ;; split: (listof Example) Sym -> (list (listof Example) (listof Example))
          (define split (split-examples examples label))
          ;; Histogram with Label Definition
          (define label-histogram (histogram (first split)))
          ;; Histogram without Label Definition
          (define not-label-histogram (histogram (second split)))
          ;; Constant Root Definition
          (define root
            (cond [(empty? (entropy-attributes (augment-histogram label-histogram attributes (length (first split)))
                                               (augment-histogram not-label-histogram attributes (length (second split))))) empty]
                  [else (best-attribute (entropy-attributes (augment-histogram label-histogram attributes (length (first split)))
                                                            (augment-histogram not-label-histogram attributes (length (second split)))))]))
          ;; (remove-symbol symbol lst) consumes a Symbol, symbol and a list of Any
          ;; lst, and removes all occurences of the symbol given.
          ;; remove-symbol: Sym (listof Any) -> (listof Any)
          (define (remove-symbol symbol lst)
            (cond [(empty? lst) empty]
                  [(and (symbol? (first lst))
                        (symbol=? symbol (first lst))) (remove-symbol symbol (rest lst))]
                  [(list? (first lst)) (cons (remove-symbol symbol (first lst))
                                             (remove-symbol symbol (rest lst)))]
                  [else (cons (first lst) (remove-symbol symbol (rest lst)))]))]
    (cond [(empty? (first (split-examples examples label))) false]
          [(empty? (second (split-examples examples label))) true]
          [(empty? (collect-attributes examples)) (cond [(> (length (first (split-examples examples label)))
                                                                    (length (second (split-examples examples label)))) true]
                                                                [else false])]
          [(equal? (build-dt (remove-symbol root (first (split-examples examples root))) label)
                   (build-dt (second (split-examples examples root)) label))
           (build-dt (remove-symbol root (first (split-examples examples root))) label)]
          [else (cons root
                      (cons (build-dt (remove-symbol root (first (split-examples examples root))) label)
                      (cons (build-dt (second (split-examples examples root)) label) empty)))])))


;; ***************************************************
;; Problem 2i
;; ***************************************************

;; (train-classifier examples label) consumes a list of Example,
;; examples and a Symbol, label and produces a function that
;; checks if the list of symbols can be attributed to an animal.

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local [;; Decision Tree Definition 
          (define d-tree (build-dt examples label))
          ;; (classifier-wrapper los) consumes a list of Symbols,
          ;; los, and produces true if the list of symbols
          ;; correspond to a decision tree and false otherwise.
          ;; classifier-wrapper: (listof Symbol) -> Bool
          (define (classifier-wrapper los)
            (classifier d-tree los))
          ;; (classifier tree los) consumes a DT, tree and list of Symbols,
          ;; los, and produces true if the list of symbols
          ;; correspond to a decision tree and false otherwise.
          ;; classifier-wrapper: DT (listof Symbol) -> Bool
          (define (classifier tree los)
            (cond [(boolean? tree) tree]
                  [(member? (first tree) los) (classifier (second tree) los)] 
                  [else (classifier (third tree) los)]))] 
    classifier-wrapper))

;; Predicates
(define goose? (train-classifier (random-animals 1000) 'goose))
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(define crow? (train-classifier (random-animals 1000) 'crow))
(define gull? (train-classifier (random-animals 1000) 'gull))

;; Tests
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(check-expect (crow? (list 'angry 'flies 'medium)) true)


;; ***************************************************
;; Bonus
;; ***************************************************

;; (performance classifier? examples label) consumes a classifier, examples,
;; and a label and computes the sensitivity and specificity for the
;; classifier on the examples.

;; Examples
(check-within (performance goose? (random-animals 1000) 'goose) (list 'goose 60 90) 20)
(check-within (performance squirrel? (random-animals 1000) 'squirrel) (list 'squirrel 80 90) 20)
(check-expect (performance goose? empty 'goose) (list 'goose 0 0))

;; performance: (listof Sym -> Bool) (listof Example) Sym -> (list Num Num)
(define (performance classifier? examples label)
  (local [(define good-animals (first (split-examples examples label)))
          (define bad-animals (second (split-examples examples label)))
          (define (remove-labels lst)
            (cond [(empty? lst) empty]
                  [else (cons (rest (first lst))
                              (remove-labels (rest lst)))]))
          (define (sensitivity class? lst)
            (cond [(empty? lst) 0]
                  [(not (class? (first lst)))
                   (sensitivity class? (rest lst))]
                  [else (+ 1 (sensitivity class? (rest lst)))]))
          (define (specificity class? lst)
            (cond [(empty? lst) 0]
                  [(class? (first lst))
                   (specificity class? (rest lst))]
                  [else (+ 1 (specificity class? (rest lst)))]))
          (define (specificity-calc spec)
            (cond [(= 0 (length bad-animals)) 0]
                  [else (* 100 (/ spec (length bad-animals)))]))
          (define (sensitivity-calc sens)
            (cond [(= 0 (length good-animals)) 0]
                  [else (* 100 (/ sens (length good-animals)))]))]
    (list label
          (round (sensitivity-calc (sensitivity classifier? (remove-labels good-animals))))
          (round (specificity-calc (specificity classifier? (remove-labels bad-animals)))))))