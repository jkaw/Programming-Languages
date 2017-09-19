#lang plai-typed

;; Ex. 11
;; check-range1 : (listof number) -> boolean
;; check to see if all numbers in a non-empty list are between 5 and 95

#|(define (check-range1 lon)
  (foldr (lambda (x) (and (< x 5) (> x 95))) lon))
(test (check-range1 (list 2 96)) false)
(test (check-range1 (list 6 8 10 50)) true)
|#

;; Ex. 12
;; convert : (listof number) -> number
;; takes a list of numbers and creates a number based on the backwards order
(define (convert lon)
  (foldr (lambda (x y) (+ x (* 10 y))) 0 lon))
(test (convert (list 1 2 3)) 321)

;; Ex. 13
;; count : (listof number) -> number
;; counts the number of items in a list
(define (count lox) : number
  (cond [(empty? lox) 0]
        [else (add1 (count (rest lox)))]))
(test (count (list 1 2 3)) 3)

;; add : (listof number) -> number
;; adds up the numbers in the list
(define (add lon) : number
  (cond [(empty? lon) 0]
        [else (+ (first lon) (add (rest lon)))]))
(test (add (list 1 2 3)) 6)

;; average-price : (list of number) -> number
;; computes the average price of a toy given a list of prices
(define (average-price lon)
  (/ (foldr + 0 lon) (count lon)))
(test (average-price (list 2 3 4)) 3)

;; Ex. 15
;; convertfc : number -> number
;; converts f to c
(define (convertfc num)
  (/ (- num 32) 1.8))
(test (convertfc 32) 0)

;; convertFC : (listof number) -> (listof number)
;; converts a list of numbers to celsius
(define (convertFC alon)
  (map (lambda (x) (* (- x 32) 5/9)) alon))

(test (convertFC (list 32 41)) (list 0 5))

;; Ex. 27
;; flatten : (listof (listof number)) -> (listof number)
#|(define (flatten lon)
  (cond [(empty? (first lon)) empty]
        [(empty? (first (first lon))) (list empty)]
        [else (append (first lon) (flatten (rest lon)))]))
(test (flatten (list (list 1 2) (list 3 4 5) (list 6))) (list 1 2 3 4 5 6))
|#

;; Ex. 28
(define (bucket lon)
  (foldr (lambda (x y) (cond [(empty? y) (cons (cons x empty) empty)]
                             [(= x (first (first y))) (cons (cons x (first y)) (rest y))]
                             [else (cons (cons x empty) y)]))
         empty lon))


#|;; Ex. 29
(define (tree-map func ft)
  (type-case ftree ft
    [Unknown (name) (Unkown "name")]
    [person (name eye-color birthyear father mother) (person (func n) eye-color birthyear (tree-map func father) (tree-map func mother))]))

;; Ex. 30
(define (add-last-name ft ln)
  (tree-map (lambda (fn) (string-append fn ln)) ft)) |#
