#lang plai-typed

(define memsize 100)



(define-type Value
  [numV (n : number)]
  [closV (param : (listof symbol)) (body : ExprC) (env : Env)]
  [boxV (loc : Location)]
  [boolV (b : boolean)]
  [symbolV (s : symbol)])


;; Our internal representation of expressions in the core language
(define-type ExprC
  [numC (n : number)]
  [varC (s : symbol)]
  [appC (fn : ExprC) (arg : (listof ExprC))]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (cond : ExprC) (then : ExprC) (else : ExprC)]
  [lamC (param : (listof symbol)) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (box : ExprC) (new : ExprC)]
  [seqC (e1 : ExprC) (e2 : ExprC)]
  [setC (var : symbol) (arg : ExprC)]
  [boolC (b : boolean)]
  [symbolC (s : symbol)]
  )


(define-type ExprS
  [numS (n : number)]
  [varS (s : symbol)]
  [appS (fname : ExprS) (arg : (listof ExprS))]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [ifS (cond : ExprS) (then : ExprS) (else : ExprS)]
  [withS (bindexpr : (listof mybind)) (body : ExprS)]
  [lamS (param : (listof symbol)) (body : ExprS)]
  [boxS (arg : ExprS)]
  [unboxS (arg : ExprS)]
  [setboxS (box : ExprS) (new : ExprS)]
  [seqS (e1 : ExprS) (e2 : ExprS)]
  [setS (var : symbol) (arg : ExprS)]
  [boolS (b : boolean)]
  [symbolS (s : symbol)]
  [withclassS (name : symbol) (svar : (listof Binding)) (ivar : (listof Binding)) (construct : (listof symbol)) (messages : (listof ExprS)) (body : ExprS)]
  [msgS (class : ExprS) (msg : symbol) (params : (listof symbol))]
  )



(define-type-alias Location number)

(define-type Binding [bind (name : symbol) (val : Location)])
(define-type-alias Env (listof Binding))
(define mtenv empty)
(define extend-env cons)

(define-type mybind [mbind (name : symbol) (val : ExprS)])

(define-type Storage [str (memory : Vector)])
(define-type-alias Store (listof Storage))
(define mtstr empty)
(define override-store cons)
(define Memory (str (vector memsize)))

(define-type Result [v*s (val : Value) (str : Store)])

(define (lookup [s : symbol] [env : Env]) : Location
  (cond [(empty? env) (error 'lookup
                             (string-append "encountered free variable "
                                            (symbol->string s)))]
        [else (if (symbol=? s (bind-name (first env)))
                  (bind-val (first env))
                  (lookup s (rest env)))]))
(test/exn (lookup 'x mtenv) "lookup: encountered free variable")
(test (lookup 'x (extend-env (bind 'y 3) (extend-env (bind 'x 2) mtenv)))
      2)

(define (fetch [loc : Location] [sto : Store]) : Value
  (cond [(> loc (sub1 memsize)) (error 'fetch "Cannot retrive value from location bigger than the Memory Size")]
        [else (vector-ref sto loc)]))

(define (isrepeated l func) 
  (cond
    [(empty? l) #f]
    [(member? (first l) (second l) func) (first l)]
    [else (isrepeated (rest l) func)]))

(define (member? x l func)
  (cond
    [(empty? l) #f]
    [(func (first l) x) #t]
    [else (member? x (rest l) func)]))



(define (boolean? [b : boolean]) : boolean
  (cond [(string=? (s-exp->string(boolean->s-exp b)) "true") #t]
        [(string=? (s-exp->string(boolean->s-exp b)) "false") #t]
        [else #f]))

(define (reachable-locations (sto : Store) (env : Env)) : (listof Location)
  (cond
    [(empty? env) empty]
    [else (append (reachable-locations-from-value sto (fetch (bind-val (first env))))
                  (reachable-locations sto (rest env)))]))

(define (reachable-locations-from-value (sto : Store) (v : Value)) : (listof Location)
  (type-case Value v
    [numV (n) empty]
    [closV (a b  e) (reachable-locations sto e)]
    [boxV (loc) (cons loc (reachable-locations-from-value sto (fetch loc sto)))]
    [boolV (b) empty]))

;; new-loc : Value  -> number
(define (new-loc [val : Value] [env : Env])
  (let ([next-loc (box -1)])
  (cond [(= (vector-length Memory) 100) (garbage-collect Memory env)]
        [else (begin (set-box! next-loc (add1 (unbox next-loc)))
                     (vector-set! (unbox next-loc) val))])))


;; Parse s-expressions into our core language
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (varS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl))
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(-) (if (= (length sl) 3)
                         (bminusS (parse (second sl)) (parse (third sl)))
                         (uminusS (parse (second sl))))]
                [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(with) ;(let ([idbind-pair (s-exp->list (second sl))])
                          (withS (isrepeated (second sl) (lambda (e1 e2) (symbol=? (first e1) (first e2)))) (parse (third sl)))]
                [(lambda) (lamS (second sl) (parse (third sl)))]
                [(box) (boxS (parse (second sl)))]
                [(unbox) (unboxS (parse (second sl)))]
                [(set-box!) (setboxS (parse (second sl)) (parse (third sl)))]
                [(begin) (foldl (lambda (e r) (seqS r (parse e)))
                                (parse (second sl))
                                (rest (rest sl)))]
                [(set!) (setS (s-exp->symbol (second sl))
                              (parse (third sl)))]
                ;[('') (symbolS (second sl))]
                [(withclass) (withclassS (second sl) (third sl)
                [else ;; an application of an identifier bound to a function def
                 (appS (parse (first sl)) (second sl))]
                )])]
              [(s-exp-list? (first sl))
               (appS (parse (first sl)) (second sl))]
              [else (error 'parse "invalide first part of s-expression")]))
    [else (error 'parse "invalid input")]]))

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [numS (n) (numC n)]
    [varS (s) (varC s)]
    [appS (f a) (appC (desugar f) (map desugar a))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    ;[bminusS (l r) (plusC (desugar l) (desugar (uminusS r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [ifS (c t e) (ifC (desugar c) (desugar t) (desugar e))]
    [withS (lob bb)
           (appC (lamC (map (lambda (x) (mbind-name x)) lob)
                                 (desugar bb))
                                (map (lambda (x) (desugar (mbind-val x))) lob))]
    [lamS (p b) (lamC p (desugar b))]
    [boxS (b) (boxC (desugar b))]
    [unboxS (b) (unboxC (desugar b))]
    [setboxS (b e) (setboxC (desugar b) (desugar e))]
    [seqS (e1 e2) (seqC (desugar e1) (desugar e2))]
    [setS (v a) (setC v (desugar a))]
    [boolS (b) (boolC b)]
    ))


;; interp : ExprC Env Store -> Value
(define (interp [e : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC e
    [numC (n) (v*s (numV n) sto)]
    [varC (s) (v*s (fetch (lookup s env) sto) sto)]
    [appC (f a)
          (type-case Result (interp f env sto)
            [v*s (v-f s-f)
                 (type-case Result (interp a env s-f)
                   [v*s (v-a s-a)
                        (let ([where (new-loc)])
                          (interp (closV-body v-f) 
                                  (extend-env (bind (closV-param v-f)
                                                    where)
                                              (closV-env v-f))
                                  (override-store (str where v-a)
                                                  s-a)))])])]
    [plusC (l r)
           (type-case Result (interp l env sto)
             [v*s (v-l s-l)
                  (type-case Result (interp r env s-l)
                    [v*s (v-r s-r)
                         (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r)
           (type-case Result (interp l env sto)
             [v*s (v-l s-l)
                  (type-case Result (interp r env s-l)
                    [v*s (v-r s-r)
                         (v*s (num* v-l v-r) s-r)])])]
    [ifC (c t e)
         (type-case Result (interp c env sto)
           [v*s (v-c s-c)
                (if (and (numV? v-c) (zero? (numV-n v-c)))
                    (interp e env s-c)
                    (interp t env s-c))])]
    [lamC (p b) (v*s (closV p b env) sto)]
    [boxC (c)
          (type-case Result (interp c env sto)
            [v*s (v-c s-c)
                 (let ([where (new-loc)])
                   (v*s (boxV where)
                        (override-store (str where v-c) s-c)))])]
    [unboxC (b)
            (type-case Result (interp b env sto)
              [v*s (v-b s-b)
                   (v*s (fetch (boxV-loc v-b) s-b)
                        s-b)])]
    [setboxC (b v)
             (type-case Result (interp b env sto)
               [v*s (v-b s-b)
                    (type-case Result (interp v env s-b)
                      [v*s (v-v s-v)
                           (v*s v-v
                                (override-store (str (boxV-loc v-b) v-v)
                                                s-v))])])
             ]
    [seqC (e1 e2)
          (type-case Result (interp e1 env sto)
            [v*s (v1 s1)
                 (interp e2 env s1)])]
    [setC (v a)
          (type-case Result (interp a env sto)
            [v*s (v-a s-a)
                 (let ([where (lookup v env)])
                   (v*s v-a
                        (override-store (str where v-a) s-a)))])]
    [boolC (b) (v*s (boolV (b)) str)]
    ))

(define (num+ [l : Value] [r : Value]) : Value
  (cond [(and (numV? l) (numV? r))
         (numV (+ (numV-n l) (numV-n r)))]
        [else (error 'num+ "operand was not a number")]))
(define (num* [l : Value] [r : Value]) : Value
  (cond [(and (numV? l) (numV? r))
         (numV (* (numV-n l) (numV-n r)))]
        [else (error 'num* "operand was not a number")]))


;; AS-swf : s-expression -> Value
(define (AS-swf s) : Value
  (let ([answer (interp (desugar (parse s)) mtenv mtstr)])
    (v*s-val answer)
    ))

(test (AS-swf '10) (numV 10))
(test (AS-swf '42) (numV 42))
(test (AS-swf '-10) (numV -10))
(test (AS-swf '(+ 1 2)) (numV 3))
(test (AS-swf '(+ -1 1)) (numV 0))
(test (AS-swf '(* 6 7)) (numV 42))
(test (AS-swf '(+ (* 6 7) 10)) (numV 52))
(test (AS-swf '(+ 10 (* 6 7))) (numV 52))
(test (AS-swf '(- 7 3)) (numV 4))
(test (AS-swf '(- (* 3 4))) (numV -12))

(test (v*s-val (interp (appC (lamC 'x (multC (varC 'x) (varC 'x))) (numC 3)) mtenv mtstr))
      (numV 9))
(test (AS-swf '((lambda x (* x x)) 3)) (numV 9))
(test (AS-swf '(with (y (* 2 3)) (* 7 y))) (numV 42))
(test (AS-swf '(if 3 3 0)) (numV 3))
(test (AS-swf '((if 3 (lambda x (* x x)) (lambda x (- x x))) 5)) (numV 25))
(test/exn (AS-swf '(((lambda f (lambda x (f 10))) (lambda y (+ x y))) 3))
          "lookup: encountered free variable x")
(test (AS-swf '(((lambda x (lambda y (+ x y))) 4) 5)) (numV 9))
(test (AS-swf '(with (f (lambda x (* x 3))) (f 3))) (numV 9))
(test (AS-swf '(with (y 5) ((lambda x (* x y)) 2))) (numV 10))

(test (AS-swf '(with (b (box 5)) (begin (set-box! b 7) (* (unbox b) (unbox b))))) (numV 49))
(test (AS-swf '(lambda x (+ x x))) (closV 'x (plusC (varC 'x) (varC 'x)) mtenv))
(test (AS-swf '(box 3)) (boxV (sub1 (new-loc)))) ;; gets first location 
(test (AS-swf '(with (x 6) (begin (set! x 2) x))) (numV 2))
(test (AS-swf '(with (b (box -1)) (set-box! b b))) (boxV (- (new-loc) 2)))
(test (AS-swf '(with (fact -1)
                     (begin (set! fact (lambda n (if n (* n (fact (- n 1))) 1)))
                            (fact 10))))
      (numV 3628800))
(test (as-swf
	(withclass ( foo (static-vars)
			   (instance-vars (v 4))
			   (constant v0)
			   (get (lambda () v0)))
	`		   (set (lambda (x) (set! v x))))
		    (with (o (make-foo 3))  
			     (begin (msg o ‘set 7)
			     (msg o ‘get)))
	)
(numV 3))
;; ((lambda (x) (* x x)) 3)
;; (lambda (x) x)

#|
(test (AS-swf '(zero 5)) 0)
(test (AS-swf '(sqr 5)) 25)
(test (AS-swf '(to-fourth (+ 3 -1))) 16)
(test/exn (AS-swf '(+ 3 y)) "lookup: encountered free variable")
(test/exn (AS-swf '(fvf 3)) "lookup: encountered free variable")
(test/exn (AS-swf '(af 3)) "lookup: encountered free variable")
|#