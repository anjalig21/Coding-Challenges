;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname html) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 06, Problem 2a
;; ***************************************************

;; =============================================================================
;; data definitions For Q2
;; =============================================================================

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag

;; A Tag is (cons Sym (listof HI))


;; =============================================================================
;; constants For Q2 examples
;; =============================================================================

(define just-text "Hello, world!")
(define short-example '(p (h1 "Heading") "Text"))
(define html-example '(html (head (title "CS135"))
                            (body (h1 "Welcome")
                                  "More text...")))


;; (html->string html-i) consumes a HI, html-i
;; and produces the equivalent HTML text.

;; Examples
(check-expect (html->string "text") "text")
(check-expect (html->string html-example)
              (string-append "<html><head><title>CS135</title></head>"
                             "<body><h1>Welcome</h1>More text...</body></html>"))

;; html->string: HI -> Str
(define (html->string html-i)
  (cond [(empty? html-i) ""]
        [(string? html-i) html-i]
        [else (apply (first html-i) (rest html-i))]))

;; Tests
(check-expect (html->string empty) "")
(check-expect (html->string short-example) "<p><h1>Heading</h1>Text</p>")
(check-expect (html->string '(i (b "i" (em "b")) "d" "c" (l "k")))
              "<i><b>i<em>b</em></b>dc<l>k</l></i>")


;; (apply first-element rest-element) consumes two elements, first-element
;; and rest-element, that can be either a symbol, string or a (listof HI)
;; and produces the equivalent HTML text.

;; Examples
(check-expect (apply 'p "Hello") "<p>Hello</p>")
(check-expect (apply (list 'p "Hello") (list 'body "How are you?"))
              "<p>Hello</p><body>How are you?</body>")

;; apply: (anyof Sym (listof HI) Str) (anyof Sym (listof HI) Str) -> Str
(define (apply first-element rest-element)
  (cond [(symbol? first-element) (string-append
                               "<" (symbol->string first-element) ">"
                               (html->string rest-element)
                               "</" (symbol->string first-element) ">")]
        [(list? first-element) (string-append (html->string first-element) (html->string rest-element))]
        [else (string-append first-element (html->string rest-element))]))


;; ***************************************************
;; Problem 2b
;; ***************************************************

;; (remove-tag sym html-i) consumes a symbol, sym and an HI,
;; html-i and removes all occurrences of that tag.

;; Examples
(check-expect (remove-tag 'b html-example) html-example)
(check-expect (remove-tag 'b '(p "Hello, " (b "World") "!")) 
              '(p "Hello, " "World" "!")) 
               
;; remove-tag: Sym HI -> (anyof HI (listof HI))
(define (remove-tag sym html-i)
  (cond [(empty? html-i) empty]
        [(string? html-i) html-i]
        [else (apply-remover sym (first html-i) (rest html-i))]))

;; Tests                    
(check-expect (remove-tag 'p '(p "Hello, " (b "World") "!")) 
              '("Hello, " (b "World") "!"))
(check-expect (remove-tag 'p (list 'p "hi")) (list "hi"))
(check-expect (remove-tag 'p '(p "Hello, " (p (p (a "World") "bob")) "!"))   
              '("Hello, "  (a "World")  "bob"  "!"))                       
(check-expect (remove-tag 'p "Hello") "Hello")
(check-expect (remove-tag 'a '(a (b "a" (em "b")) "c" (l "a")))
              '((b "a" (em "b")) "c" (l "a")))
(check-expect (remove-tag 'p '(p (p) (p) (p))) empty)
(check-expect (remove-tag 'html "Hello") "Hello")


;; (apply-remover sym firsth resth) consumes a symbol, sym, a symbol or HI,
;; firsth, and a symbol, string or (listof HI), resth, and removes all
;; occurrences of that symbol (i.e. tag).

;; Examples
(check-expect (apply-remover 'a (list 'a "hello") (list 'a (list 'b "world")))
              (list "hello" (list 'b "world")))
(check-expect (apply-remover 'a (list 'a) (list 'a (list 'b "world")))
              (list (list 'b "world")))

;; apply-remover: Sym (anyof Sym HI) (anyof Sym Str (listof HI)) -> (anyof HI (listof HI))
(define (apply-remover sym first-element rest-element)
  (cond [(and (symbol? first-element) (symbol=? sym first-element)) (remove-tag sym rest-element)]
        [(and (list? first-element) (symbol=? (first first-element) sym))
         (append (remove-tag sym (rest first-element)) (remove-tag sym rest-element))]
        [else (cons first-element (remove-tag sym rest-element))]))


;; ***************************************************
;; Problem 2c
;; ***************************************************

;; (okay-tags? html-i) consumes an HI, html-i and produces true if it has
;; followed the rules, and produces false otherwise.

;; Examples
(check-expect (okay-tags? html-example) true)
(check-expect (okay-tags? '(body (hr "hello"))) false)
(check-expect (okay-tags? '(body (li "Q1") "text")) false)

;; okay-tags?: HI -> Bool
(define (okay-tags? html-i)
  (valid-tag? 'empty html-i))
        
;; Tests
(check-expect (okay-tags? '(ol (li "Q1") "text")) true)
(check-expect (okay-tags? '(ul (li "Q1") "text")) true)
(check-expect (okay-tags? '(ol "hello")) true)
(check-expect (okay-tags? '(ul "hello")) true)
(check-expect (okay-tags? '(body (hr))) true)
(check-expect (okay-tags? '(html "hi" (hr "text"))) false)
(check-expect (okay-tags? '(hr (b "hi"))) false)
(check-expect (okay-tags? '(hr)) true)
(check-expect (okay-tags? '(html "bye" (hr))) true)
(check-expect (okay-tags? '(ol (li))) true)
(check-expect (okay-tags? '(ul "bye" (li))) true)
(check-expect (okay-tags? '(li (ul))) false)
(check-expect (okay-tags? '(ul "hello" (b (li)))) false)
(check-expect (okay-tags? '(a "Hello" )) true)
(check-expect (okay-tags? "Hello")  true)


;; (valid-tag? prev-tag html-i) consumes a symbol,
;; prev-tag, a HI, html-i, and and produces true if it has
;; followed the rules, and produces false otherwise.

;; Examples
(check-expect (valid-tag? 'ol '(li "hi")) true)
(check-expect (valid-tag? 'body '(li "hi")) false)

;; valid-tag: Sym HI HI -> Bool
(define (valid-tag? prev-tag html-i)
  (cond [(empty? html-i) true]
        [(string? html-i) true]
        [else (check-tags? prev-tag (first html-i) (rest html-i))]))


;; (check-tags prev-tag firsth resth) consumes a symbol, prev-tag,
;; and a HI, firsth and resth, and produces true if it has
;; followed the rules, and produces false otherwise.

;; Examples
(check-expect (check-tags? 'empty 'ol '(li "hello")) true)
(check-expect (check-tags? 'empty 'body '(li "hello")) false)

;; check-tags: HI HI -> Bool
(define (check-tags? prev-tag firsth resth)
  (cond [(and (symbol? firsth)
              (symbol=? firsth 'hr)) (cond [(not (empty? resth)) false]
                                           [else (valid-tag? 'hr resth)])]
        [(and (symbol? firsth)
              (symbol=? firsth 'li)) (cond [(and (not (symbol=? prev-tag 'ol))
                                                 (not (symbol=? prev-tag 'ul))) false]
                                           [else (valid-tag? 'li resth)])]
        [(symbol? firsth) (valid-tag? firsth resth)]
        [(list? firsth) (and (valid-tag? prev-tag firsth)
                             (valid-tag? prev-tag resth))]
        [else (valid-tag? prev-tag resth)]))