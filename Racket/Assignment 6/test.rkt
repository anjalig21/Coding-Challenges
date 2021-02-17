;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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

;; (collect-attributes examples) consumes a list of Examples,
;; examples, and produces a list of attributes contained in
;; the examples with no duplicates.

;; Examples
(check-expect (collect-attributes seen)
              (list 'small 'large 'swims 'medium 'flies 'angry))
(check-expect (collect-attributes squirrel-list)
              (list 'small 'angry))

;; collect-attributes: (listof Examples) -> (listof Sym)
(define (collect-attributes examples)
  (local [;; (remove-duplicates lst) consumes a list of any, and
          ;; removes any duplicates.
          ;; remove-duplicates (listof Any) -> (listof Any)
          (define (remove-duplicates lst)
                   (cond [(empty? lst) empty]
                         [(member? (first lst) (rest lst)) (remove-duplicates (rest lst))]
                         [else (cons (first lst) (remove-duplicates (rest lst)))]))
          ;; (all-attributes lst) consumes a listof Examples, lst, and produces
          ;; a list of attributes contained in the examples with duplicates.
          ;; all-attributes: (listof Examples) -> (listof Sym)
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

;; (split-examples examples symbol) consumes a list of Examples, examples
;; and a Symbol, symbol and splits the list of examples on the
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

;; split-examples: (listof Examples) Sym -> (listof (listof Examples) (listof Examples))
(define (split-examples examples symbol)
  (local [;; (symbol-in-list symbol lst) consumes a Symbol, symbol and a
          ;; list of Examples, lst, and outputs a list of all the lists with
          ;; that symbol.
          ;; symbol-in-list: Sym (listof Examples) -> (listof Examples)
          (define (symbol-in-list symbol lst)
            (cond [(empty? lst) empty]
                  [(member? symbol (first lst)) (cons (first lst) (symbol-in-list symbol (rest lst)))]
                  [else (symbol-in-list symbol (rest lst))]))
          ;; (not-symbol-list symbol lst) consumes a Symbol, symbol and a
          ;; list of Examples, lst, and outputs a list of all the lists that
          ;; don't have that symbol.
          ;; not-symbol-list: Sym (listof Examples) -> (listof Examples)
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

;; (histogram examples) consumes a list of Examples, examples
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

;; histogram: (listof Examples) -> Histogram
(define (histogram examples)
  (local [;; (count-attributes symbol examples) consumes a Symbol, symbol, and
          ;; a list of Examples, examples and produces the amount of times that
          ;; symbol appears in the list.
          ;; count-attributes: Sym (listof Examples) -> Nat
          (define (count-attributes symbol examples)
            (cond [(empty? examples) 0]
                  [(list? (first examples)) (+ (count-attributes symbol (first examples))
                                               (count-attributes symbol (rest examples)))]
                  [(symbol=? symbol (first examples)) (+ 1 (count-attributes symbol (rest examples)))]
                  [else (count-attributes symbol (rest examples))]))
          ;; (combine-histogram listof-attributes examples) consumes a list of Symbols, listof-attributes,
          ;; and a list of Examples, examples and produces the Histogram.
          ;; count-attributes: (listof Sym) (listof Examples) -> Histogram
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

;; augment-histogram: Histogram (listof Sym) Nat -> Augemented Histogram
(define (augment-histogram histogram attributes total)
  (local [;; (symbol-in-histogram? symbol histogram) consumes a Symbol, symbol, and
          ;; a Histogram, histogram, and outputs the value beside the symbol if it exists,
          ;; otherwise it produces 0.
          ;; symbol-in-histogram: Sym Histogram -> Nat
          (define (symbol-in-histogram symbol histogram)
            (cond [(empty? histogram) 0]
                  [(list? (first histogram)) (+ (symbol-in-histogram symbol (first histogram))
                                                 (symbol-in-histogram symbol (rest histogram)))]
                  [(and (symbol? (first histogram))
                        (symbol=? (first histogram) symbol)) (second histogram)]
                  [else (symbol-in-histogram symbol (rest histogram))]))]
            (cond [(empty? attributes) empty]
                  [else (cons (list (first attributes)
                                    (symbol-in-histogram (first attributes) histogram)
                                    (- total (symbol-in-histogram (first attributes) histogram)))
                              (augment-histogram histogram (rest attributes) total))])))

;; Tests
;; ADD TESTS HERE!!!!!!!!!!!!!!!!!!!!!!!!!    
                                             

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

;; (build-dt examples label) consumes a list of Examples, examples
;; and a Symbol, label, and then produces a decision tree.

;; Examples
(check-expect (build-dt (random-animals 1000) 'goose)
              (list 'large (list 'swims (list 'angry true false) false) false))
(check-expect (build-dt (random-animals 1000) 'crow)
              (list 'swims false (list 'flies (list 'angry (list 'medium true (list 'large true (list 'small true false))) false) false)))
(check-expect (build-dt (random-animals 1000) 'emu) false)


(define (build-dt examples label)
  (local [(define attributes (collect-attributes examples))
          (define split (split-examples examples label))
          (define label-histogram (histogram (first split)))
          (define not-label-histogram (histogram (second split)))
          (define (remove-symbol symbol lst)
            (cond [(empty? lst) empty]
                  [(and (symbol? (first lst))
                        (symbol=? symbol (first lst))) (remove-symbol symbol (rest lst))]
                  [(list? (first lst)) (cons (remove-symbol symbol (first lst))
                                             (remove-symbol symbol (rest lst)))]
                  [else (cons (first lst) (remove-symbol symbol (rest lst)))]))]
    (cond [(empty? examples) empty]
          [(empty? (first (split-examples examples label))) false]
          [(empty? (second (split-examples examples label))) true]
          [(empty? (collect-attributes examples)) (cond [(> (length (first (split-examples examples label)))
                                                                    (length (second (split-examples examples label)))) true]
                                                                [else false])]
          [else (cons (build-dt (remove-symbol (best-attribute
                                                (entropy-attributes (augment-histogram label-histogram attributes (length (first split)))
                                                                    (augment-histogram not-label-histogram attributes (length (second split)))))
                                               (first (split-examples examples (best-attribute
                                                                                (entropy-attributes (augment-histogram label-histogram attributes (length (first split)))
                                                                                                    (augment-histogram not-label-histogram attributes (length (second split)))))))) label)
                      (build-dt (second (split-examples examples (best-attribute
                                                                  (entropy-attributes (augment-histogram label-histogram attributes (length (first split)))
                                                                                      (augment-histogram not-label-histogram attributes (length (second split))))))) label))])))