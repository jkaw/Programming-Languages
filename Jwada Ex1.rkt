#lang plai-typed

;; Ex.1
;; sum-coins : To detemine the sum of the value of the coins provided in cents
;; number number number number -> number
(define (sum-coins pennies nickles dimes quarters)
  (+
   (+ (* 25 quarters) (* 10 dimes))
   (+ (* 5 nickles) pennies)))
(test (sum-coins 1 1 1 1) 41)

;; Ex. 2
;; area-cylinder : to determine the area of a cylinder based on the radius of the base and the height of the cylinder
;; number number -> number

(define pi 3.14)
(define (area-cylinder radius height)
  (* (* pi (* radius radius)) height))
(test (area-cylinder 2 2) 25.12)

;; Ex.3
;; area-pipe : to determine the surface area of a pipe with its inner radius, thickness, and height
;; number number number -> number

(define (area-pipe radius thickness height)
  (-
   (* (+ thickness radius) (* 2 pi))
   (* radius (* 2 pi))))
(test (area-pipe 1 1 1) 6.28)
;; surface-area : determines the surface area of a cylinder
;; number number -> number
(define (surface-area radius height)
  (* radius (* 2 pi)))

;; area-pipe-dos : compute surface area of pipe using other functions
;; number number number -> number
(define (area-pipe-dos radius thickness height)
  (-
   (surface-area (+ radius thickness) height)
   (surface-area radius height)))
(test (area-pipe-dos 1 1 1) 6.28)

;; Ex.4
;; tax : determines the amount of tax owed on an amount of salary
;; number -> number
(define (tax salary)
  (cond [(<= salary 240) 0]
        [(and (> salary 240) (<= salary 480)) (* salary .15)]
        [else (* salary .28)]))
(test (tax 100) 0)
(test (tax 250) 37.5)
(test (tax 500) 140)

;; netpay : determines what the payout is after tax
;; number -> number
(define (netpay hours)
  (-
   (* hours 12)
   (tax (* hours 12))))
(test (netpay 1) 12)

;; Ex.5
;; what-kind : determines what kind of quadratic we have based on the coefficients
;; number number number -> symbol
(define (what-kind a b c)
  (cond [(zero? a) 'degenerate]
        [(> (* b b) (* (* 4 a) c)) 'two]
        [(= (* b b) (* (* 4 a) c)) 'one]
        [(< (* b b) (* (* 4 a) c)) 'none]))
(test (what-kind 1 4 4) 'one)
(test (what-kind 0 1 1) 'degenerate)
(test (what-kind 1 2 20) 'none)
(test (what-kind 1 10 2) 'two)

;; Ex.6
(define-type time-point
  [time (hours : number) (minutes : number) (seconds : number)])

;; time-diff : time time -> number
;; comsumes two times and gives the difference in seconds
(define (time-diff t1 t2)
  (-
   (+ (time-seconds t1) (+ (* (time-hours t1) 3600) (* (time-minutes t1) 60)))
   (+ (time-seconds t2) (+ (* (time-hours t2) 3600) (* (time-minutes t2) 60)))))
(test (time-diff (time 0 0 50) (time 0 0 30)) 20)

;;Ex. 7
(define-type posn
  [point (x : number) (y : number)])

(define-type shape
  [circle (center : posn) (radius : number)]
  [square (ULC : posn) (length : number)]
  [rectangle (ULC : posn) (width : number) (height : number)])


;; Ex.8
;; area : shape -> number
;; consumes a shape and returns the area of the shape
(define (area s)
  (type-case shape s
    [circle (center radius) (* pi (* radius radius))]
    [square (ULC length) (* length length)]
    [rectangle (ULC width height) (* width height)]))
(test (area (circle (point 0 0) 2)) (* 4 3.14))
(test (area (square (point 0 0) 2)) 4)
(test (area (rectangle (point 0 0) 2 2)) 4)

;; Ex.9
;; translate-shape : shape number -> shape
;; moves the position of the given shape in the x direction by the given number of units.
(define (translate-shape s d)
  (type-case shape s
    [circle (center radius) (circle (point (+ (point-x center) d) (point-y center)) radius)]
    [square (ULC length) (square (point (+ (point-x ULC) d) (point-y ULC)) length)]
    [rectangle (ULC width height) (rectangle (point (+ (point-x ULC) d) (point-y ULC)) width height)]))
(test (translate-shape (circle (point 0 0) 2) 1) (circle (point 1 0) 2))
(test (translate-shape (square (point 0 0) 2) 1) (square (point 1 0) 2))
(test (translate-shape (rectangle (point 0 0) 2 2) 1) (rectangle (point 1 0) 2 2))

;; Ex.10
;; in-shape? : shape posn -> boolean
;; determines whether of not a posn in in the area of a shape
(define (in-shape? s p)
  (type-case shape s
    [circle (center radius)
            (cond [(> (+ (* (- (point-x p) (point-x center)) (- (point-x p) (point-x center))) (* (- (point-y p)  (point-y center)) (- (point-y p)  (point-y center))))
                      (+ (* (- (point-x center) (- (point-x center) radius)) (- (point-x center) (- (point-x center) radius))) (* (- (point-y center) (point-y center)) (- (point-y center) (point-y center)))))
                   false]
                  [else true])]
    [square (ULC length)
            (cond [(> (point-x p) (+ (point-x ULC) length)) false]
                  [(< (point-x p) (point-x ULC)) false]
                  [(< (point-y p) (- (point-y ULC) length)) false]
                  [(> (point-y p) (point-y ULC)) false]
                  [else true])]
    [rectangle (ULC width height)
               (cond [(> (point-x p) (+ (point-x ULC) width)) false]
                  [(< (point-x p) (point-x ULC)) false]
                  [(< (point-y p) (- (point-y ULC) height)) false]
                  [(> (point-y p) (point-y ULC)) false]
                  [else true])]))
(test (in-shape? (circle (point 0 0) 2) (point 0 1)) true)
(test (in-shape? (circle (point 0 0) 2) (point 2 1)) false)
(test (in-shape? (square (point 0 0) 2) (point 0 1)) false) 
(test (in-shape? (square (point 0 0) 2) (point 0 -1)) true)
(test (in-shape? (rectangle (point 0 0) 2 2) (point 0 1)) false)
(test (in-shape? (rectangle (point 0 0) 2 2) (point 0 -1)) true)