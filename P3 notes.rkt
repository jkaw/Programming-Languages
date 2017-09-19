#lang plai

(define (msg o m . a)
  (apply (o m) a))

;; basic objects
(define o
  (lambda (m) (case m
               [(inc) (lambda (x) (+ x 1))]
               [(dec) (lambda (x) (- x 1))])))

(test (msg o 'inc 5) 6)

;; constructors
(define o-contr
  (lambda ()
    (lambda (m)
      (case m
        [(inc) (lambda (x) (+ x 1))]
        [(dec) (lambda (x) (- x 1))]))))

(test (let ([o1 (o-contr)]
            [o2 (o-contr)])
        (+ (msg o1 'inc 6) (msg o2 'dec 3)))
      9)
        
;; state

(define o-state
  (lambda (init)
    (let ([balance init])
      (lambda (m)
        (case m
          [(inc) (lambda () (set! balance (+ balance 1)))]
          [(dec) (lambda () (set! balance (- balance 1)))]
          [(get) (lambda () balance)]
          [(initial-value) (lambda () init)]
          [(reset) (lambda() (set! balance init))])))))

(test (let ([o1 (o-state 100)]
            [o2 (o-state 20)])
        (begin (msg o1 'inc)
               (msg o2 'dec)
               (+ (msg o1 'get) (msg o2 'get))))
      120)

;; private members


;; static members
(define o-static
  (let ([idcount 0])
    (lambda (init)
      (let ([balance init]
            [myid idcount])
        (begin (set! idcount (add1 idcount))
               (lambda (m)
                 (case m
                   [(inc) (lambda () (set! balance (+ balance 1)))]
                   [(dec) (lambda () (set! balance (- balance 1)))]
                   [(get) (lambda () balance)]
                   [(myid) (lambda () myid)]
                   [(idcount) (lambda () idcount)])))))))
(test (let ([o1 (o-static 20)]
            [o2 (o-static 50)])
        (begin (msg o1 'inc)
               (msg o2 'dec)
               (and (= (msg o1 'get) 21)
                    (= (msg o2 'get) 49)
                    (= (msg o1 'myid) 0)
                    (= (msg o2 'myid) 1)
                    (= (msg o1 'idcount) 2)
                    (= (msg o2 'idcount) 2))))
      #true)


;; Objects with self-reference using mutation
(define o-self!
  (let ([self 'dummy])
    (begin (set! self (lambda (m)
                        (case m
                          [(first) (lambda (x) (msg self 'second (add1 x)))]
                          [(second) (lambda (x) (add1 x))])))
           self)))
(test ((o-self! 'first) 3) 5)


;; Objects with self-reference w/o mutation
(define o-self-no!
  (lambda (m)
    (case m
      [(first) (lambda (self x) ((self 'second) self (add1 x)))]
      [(second) (lambda (self x) (add1 x))])))

(test ((o-self-no! 'first) o-self-no! 3) 5)

(define (msg/self o m . a)
  (apply (o m) o a))

(test (msg/self o-self-no! 'first 3) 5)


;; dynamic dispatch
(define (mt)
  (lambda (m)
    (case m
      [(sum) (lambda () 0)])))

(define (node v l r)
  (lambda (m) (case m
                [(sum) (lambda () (+ v (msg l 'sum) (msg r 'sum)))])))

(define bt (node 5 (node 3 (mt) (mt)) (node 9 (node 7 (mt) (mt)) (mt))))

(test (msg bt 'sum) 24)

;; Classes/extensions
(define (mt/size parent-maker)
  (let ([parent-object (parent-maker)])
    (lambda (m)
      (case m
        [(size) (lambda () 0)]
        [else (parent-object m)]))))

(define (node/size parent-maker v l r)
  (let ([parent-object (parent-maker v l r)])
    (lambda (m)
      (case m
        [(size) (lambda () (+ 1 (msg l 'size) (msg r 'size)))]
        (else (parent-object m))))))

(define btsize (node/size node 5
                          (node/size node 3 (mt/size mt) (mt/size mt))
                          (node/size node 9
                                     (node/size node 7 (mt/size mt) (mt/size mt)) (mt/size mt))))
(test (msg btsize 'size) 4)
(test (msg btsize 'sum) 24)


;; desugar cons into lambda, desugar first and rest into an application
      
                   
