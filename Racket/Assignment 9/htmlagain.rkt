;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htmlagain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; ***************************************************
;; Anjali Gupta (20889989)
;; CS 135 Fall 2020
;; Assignment 09, Problem 3a
;; ***************************************************

;; An HTML-Item (HI) is one of
;; * Str
;; * Tag
;; A Tag is (cons Sym (listof HI))

;; (tokenize string) consumes a well-formed string, string, corresponding to
;; an HTML item, and produces a list of strings representing the opening tags,
;; closing tags, and strings in the document.

;; Examples
(check-expect (tokenize "<p><h1>Heading</h1>Text</p>")
              '("<p>" "<h1>" "Heading" "</h1>" "Text" "</p>"))
(check-expect (tokenize "a b c") '("a b c"))

;; tokenize: Str -> (listof Str)
(define (tokenize string)
  (local [;; (HI->los loc) consumes a list of characters,
          ;; loc and produces a list of strings relating
          ;; to a HI according to the list of characters.
          ;; HI->los: (listof Char) -> (listof Str)
          (define (HI->los loc)
            (local [;; (first-tag loc) consumes a list of characters,
                    ;; loc and produces the first tag.
                    ;; first-tag: (listof Char) -> (listof Char)
                    (define (first-tag loc)
                      (cond [(char=? (first loc) #\>) (cons (first loc) empty)]
                            [else (cons (first loc) (first-tag (rest loc)))]))
                    ;; (rest-tags loc) consumes a list of characters,
                    ;; loc and produces the rest of the tags.
                    ;; rest-tags: (listof Char) -> (listof Char)
                    (define (rest-tags loc)
                      (cond [(char=? (first loc) #\>) (rest loc)]
                            [else (rest-tags (rest loc))]))
                    ;; (rest-tags-after-element loc) consumes a
                    ;; list of characters, loc, and produces the
                    ;; rest of the tags after some text.
                    ;; rest-tagsafter-element: (listof Char) -> (listof Char)
                    (define (rest-tags-after-element loc)
                      (cond [(empty? loc) empty]
                            [(char=? (first loc) #\<) (cons (first loc) (rest loc))]
                            [else (rest-tags-after-element (rest loc))]))
                    ;; (first-element loc) consumes a list of characters, loc,
                    ;; and produces the first element (i.e. some text).
                    ;; first-element: (listof Char) -> (listof Char)
                    (define (first-element loc)
                      (cond [(empty? loc) empty]
                            [(char=? (first loc) #\<) empty]
                            [else (cons (first loc) (first-element (rest loc)))]))]
            (cond [(empty? loc) empty]
                  [(char=? (first loc) #\<) (cons (list->string (first-tag loc)) (HI->los (rest-tags loc)))]
                  [else (cons (list->string (first-element loc)) (HI->los (rest-tags-after-element loc)))])))]
    (HI->los (string->list string))))

;; Tests
(check-expect (tokenize "") empty)
(check-expect (tokenize "<p>Text</p>") '("<p>" "Text" "</p>"))
(check-expect (tokenize "<p><h1><button>Click!</button>Heading</h1>Text</p>")
              '("<p>" "<h1>" "<button>" "Click!" "</button>" "Heading" "</h1>" "Text" "</p>"))

;; ***************************************************
;; Problem 3b
;; ***************************************************

;; (string->html string) consumes a non-empty, well-formed string
;; representing an HTML item, and produces an HI representing that item. 

;; Examples:
(check-expect (string->html "<p><h1>Heading</h1>Text</p>")
              '(p (h1 "Heading") "Text"))
(check-expect (string->html "") "")

;; string->html: Str -> HI 
(define (string->html string)
  (local [;; (change-end-tag los) consumes a list of strings, los,
          ;; and produces an HI representing that item.
          ;; change-end-tag: (listof Str) -> HI
         (define (change-end-tag los)
            (cond [(empty? los) empty]
                  [(string=? "<" (substring (first los) 0 1))
                   (process-tag (rest los) empty (first los) 0)]
                  [else (cons (first los) (change-end-tag (rest los)))]))
          ;; (process-tag los element-inside end-tag) consumes two list
          ;; of strings, los and element-inside, and a string, end-tag,
          ;; and produces an HI representing that item.
          ;; process-tag: (listof Str) (listof Str) Str -> HI
          (define (process-tag los element-inside start-tag count)
            (local [(define end-tag (string-append "</" (substring start-tag 1)))]
              (cond [(string=? (first los) start-tag)
                     (process-tag (rest los) (append element-inside (list (first los))) start-tag (add1 count))]
                    [(string=? (first los) end-tag)
                     (cond [(= count 0)
                            (cons (cons (string->symbol (substring end-tag 2 (sub1 (string-length end-tag))))
                                        (change-end-tag element-inside))
                                  (change-end-tag (rest los)))]
                           [else (process-tag (rest los) (append element-inside (list (first los))) start-tag (sub1 count))])]
                    [else (process-tag (rest los) (append element-inside (list (first los))) start-tag count)])))]
    
    (cond [(string=? string "") ""]
          [else (first (change-end-tag (tokenize string)))])))

;; Tests
(check-expect (string->html "<p><h1><button>Click!</button>Heading</h1>Text</p>")
              '(p (h1 (button "Click!") "Heading") "Text"))
(check-expect (string->html "Text") "Text")
(check-expect (string->html "<title><p>Text</p></title>") '(title (p "Text")))
(check-expect (string->html "<title></title>") (list 'title))
(check-expect (string->html "<p><h1><h1>Heading</h1></h1>Text</p>") '(p (h1 (h1 "Heading")) "Text"))