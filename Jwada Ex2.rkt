#lang plai-typed

;; Ex. 11
;; check-range1 : (listof number) -> boolean
;; check to see if all numbers in a non-empty list are between 5 and 95

(define (check-range1 lon)
  (cond [(empty? lon) true]
        [(or (> (first lon) 95) (< (first lon) 5)) false]
        [else (check-range1 (rest lon))]))
(test (check-range1 (list 2)) false)
(test (check-range1 (list 6 8 10 50)) true)

;; check-range : (listof number) number number -> boolean
;; check to see if numbers in list are in the range specified
(define (check-range (lon : (listof number)) up low) : boolean
  (cond [(empty? lon) true]
        [(or (> (first lon) up) (< (first lon) low)) false]
        [else (check-range (rest lon) up low)]))
(test (check-range (list 2) 30 3) false)
(test (check-range (list 6 8 10 50) 95 5) true)

;; Ex. 12
;; convert : (listof number) -> number
;; takes a list of numbers and creates a number based on the backwards order
(define (convert lon)
  (cond [(empty? lon) 0]
        [else (+ (first lon) (* (convert (rest lon)) 10))]))
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
  (/ (add lon) (count lon)))
(test (average-price (list 2 3 4)) 3)

;; Ex. 15
;; convertfc : number -> number
;; converts f to c
(define (convertfc num)
  (/ (- num 32) 1.8))
(test (convertfc 32) 0)

;; convertFC : (listof number) -> (listof number)
;; converts a list of numbers to celsius
(define (convertFC lon)
  (cond [(empty? lon) empty]
        [else (cons (convertfc (first lon)) (convertFC (rest lon)))]))
(test (convertFC (list 32 41)) (list 0 5.0))

;; Ex. 16
;; suffixes : (listof symbol) -> (listof (listof symbol))
;; creates a list of suffixes of the original list
(define (suffixes los)
  (cond [(empty? los) (list empty)]
        [else (cons los (suffixes (rest los)))]))
(test (suffixes (list 'a 'b 'c)) (list (list 'a 'b 'c) (list 'b 'c) (list 'c) (list)))

;; Ex. 17
(define-type ftree
  [person (name : string) (birthyear : number) (eyecolor : symbol) (father : ftree) (mother : ftree)]
  [Unknown (name : string)])

;; Ex. 18
;; count-persons : ftree -> number
;; counts the amount of people in a ftree
(define (count-persons tree)
  (type-case ftree tree
    [person (name birthyear eyecolor father mother)
            (+ 1
               (+ (count-persons (person-father tree))
                  (count-persons (person-mother tree))))]
    [Unknown (name) 0]))
(test (count-persons (person "jared" 1995 'brown (Unknown "unknown") (person "gayle" 1958 'brown (Unknown "unknown") (Unknown "unknown")))) 2)

;; Ex. 19

;; age-sum : ftree number-> number
;; adds the age from all members of a ftree
(define (add-sum f n)
  (type-case ftree f
    [person (name birthyear eyecolor father mother)
            (+ (- n (person-birthyear f))
               (+
                (add-sum mother n)
                (add-sum father n)))]
    [Unknown (name) 0]))
(test (add-sum (person "jared" 1995 'brown (Unknown "unknown") (person "gayle" 1995 'brown (Unknown "unknown") (Unknown "unknown"))) 2015) 40)

;; average-age : ftree number -> number
;; takes a ftree and a year and returns the average age of the ftree
(define (average-age ftree n)
  (/ (add-sum ftree n) (count-persons ftree)))
(test (average-age (person "jared" 1995 'brown (Unknown "unknown") (person "gayle" 1995 'brown (Unknown "unknown") (Unknown "unknown"))) 2015) 20)

;; Ex. 20
;; eye-colors : ftree -> (listof symbol)
;; creates a list of the eye colors in a ftree
(define (eye-colors f)
  (type-case ftree f
    [person (name birthyear eyecolor father mother)
            (append (list eyecolor)
                    (append
                     (eye-colors father)
                     (eye-colors mother)))]
    [Unknown (name) empty]))
(test (eye-colors (person "jared" 1995 'brown (Unknown "unknown") (person "gayle" 1995 'brown (Unknown "unknown") (Unknown "unknown")))) (list 'brown 'brown))


