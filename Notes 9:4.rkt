#lang plai-typed

(define (count-persons l)
  (cond [(empty? l) 0]
        [else (add1 (count-persons (rest l)))]))
(define (my-fold f b l)
  (cond [(empty? l) b]
        [else (f (first l) (my-fold f b (rest l)))]))
(define (cp-f l)
  (my-fold (lambda (i a) (add1 a)) 0 l))

(test (cp-f (list 1 2 3 4 5)) 5)

(define (my-map f l)
  (cond [(empty? l) empty]
        [else (cons (f (first l)) (my-map f (rest l)))]))

(test (my-map add1 (list 1 2 3 4)) (list 2 3 4 5))

(define (my-filter bf l)
  (cond [(empty? l) empty]
        [(bf (first l)) (cons (first l) (my-filter bf (rest l)))]
        [else (my-filter bf (rest l))]))

(define (drop-exp t l)
  (my-filter (lambda (x) (<= x t)) l))


(define-type MLON
  [mt]
  [mc (front : number) (others : MLON)])

(define (mcons [x : number] [y : MLON]) : MLON
  (mc x y))
(define (mfirst [l : MLON]) : number
  (mc-front l))
(define (mrest [l : MLON]) : MLON
  (mc-others l))


(test (drop-exp 5 (list 2 10 4 6)) (list 2 4))

#|
<ex> : number | symbol
     | (<op> <expr> <expr>)
     | (with (<symbol> <expr>) <expr>)
     | if (<expr> <expr> <expr>)
<op> : + | * | symbol


|#
#|
(define-type Aexpr
  [num (N : number)]
  [add (l : Aexpr) (r : Aexpr)]
  [mul (l : Aexpr) (r : Aexpr)])
(define (parse sexpr)
  (cond [(s-exp-number? sexpr) (num (s-exp->number sexpr))]
       ; [(s-exp-symbol? sexpr) ...]
        [(s-exp-list? sexpr)
         (let ((exp (s-exp->list sexpr))
               (op (s-exp->symbol (first exp))))
           (cond [(symbol=? op '+) (add (parse (second exp))
                                        (parse (third exp)))]
                 [(symbol=? op '*) ...]
                 [else (error 'parse "invalid expression")]))]))
           
|#
(local ((define (f x) (+ x 5))
	(define (g alon)
	  (cond
	    [(empty? alon) empty]
	    [else (cons (f (first alon)) (g (rest alon)))])))
  (g (list 1 2 3)))
