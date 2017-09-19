#lang plai-typed

(define-type Value
  [numV (n : number)]
  [closV (param : symbol) (body : ExprC) (env : Env)]
  [boxV (loc : Location)]
  [objV (names : (listof symbol)) (vals : (listof Value))])

;; Our internal representation of expressions in the core language
(define-type ExprC
  [numC (n : number)]
  [varC (s : symbol)]
  [appC (fn : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (cond : ExprC) (then : ExprC) (else : ExprC)]
  [lamC (param : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (box : ExprC) (new : ExprC)]
  [seqC (e1 : ExprC) (e2 : ExprC)]
  [setC (var : symbol) (arg : ExprC)]
  [objC (names : (listof symbol)) (vals : (listof ExprC))]
  [msgC (obj : ExprC) (msg : symbol)]
  )


(define-type ExprS
  [numS (n : number)]
  [varS (s : symbol)]
  [appS (fname : ExprS) (arg : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [ifS (cond : ExprS) (then : ExprS) (else : ExprS)]
  [withS (id : symbol) (bindexpr : ExprS) (body : ExprS)]
  [lamS (param : symbol) (body : ExprS)]
  [boxS (arg : ExprS)]
  [unboxS (arg : ExprS)]
  [setboxS (box : ExprS) (new : ExprS)]
  [seqS (e1 : ExprS) (e2 : ExprS)]
  [setS (var : symbol) (arg : ExprS)]
  [objS (names : (listof symbol)) (vals : (listof ExprS))]
  [msgS (obj : ExprS) (msg : symbol) (arg : ExprS)]
  )

(define-type-alias Location number)

(define-type Binding [bind (name : symbol) (val : Location)])
(define-type-alias Env (listof Binding))
(define mtenv empty)
(define extend-env cons)

(define-type Storage [str (loc : Location) (val : Value)])
(define-type-alias Store (listof Storage))
(define mtstr empty)
(define override-store cons)

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
  (cond [(empty? sto) (error 'fetch
                             "encountered uninitialize location")]
        [else (if (= loc (str-loc (first sto)))
                  (str-val (first sto))
                  (fetch loc (rest sto)))]))

;; new-loc :  -> number
(define new-loc
  (let ([next-loc (box -1)])
    (lambda () (begin (set-box! next-loc (add1 (unbox next-loc)))
                      (unbox next-loc)))))


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
                [(with) (let ([idbind-pair (s-exp->list (second sl))])
                          (withS (s-exp->symbol (first idbind-pair))
                                 (parse (second idbind-pair))
                                 (parse (third sl))))]
                [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))]
                [(box) (boxS (parse (second sl)))]
                [(unbox) (unboxS (parse (second sl)))]
                [(set-box!) (setboxS (parse (second sl)) (parse (third sl)))]
                [(begin) (foldl (lambda (e r) (seqS r (parse e)))
                                (parse (second sl))
                                (rest (rest sl)))]
                [(set!) (setS (s-exp->symbol (second sl))
                              (parse (third sl)))]
                [else ;; an application of an identifier bound to a function def
                 (appS (parse (first sl)) (parse (second sl)))]
                )]
              [(s-exp-list? (first sl))
               (appS (parse (first sl)) (parse (second sl)))]
              [else (error 'parse "invalide first part of s-expression")]))]
    [else (error 'parse "invalid input")]))

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [numS (n) (numC n)]
    [varS (s) (varC s)]
    [appS (f a) (appC (desugar f) (desugar a))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    ;[bminusS (l r) (plusC (desugar l) (desugar (uminusS r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [ifS (c t e) (ifC (desugar c) (desugar t) (desugar e))]
    [withS (id be bb)
           (appC (lamC id (desugar bb)) (desugar be))]
    [lamS (p b) (lamC p (desugar b))]
    [boxS (b) (boxC (desugar b))]
    [unboxS (b) (unboxC (desugar b))]
    [setboxS (b e) (setboxC (desugar b) (desugar e))]
    [seqS (e1 e2) (seqC (desugar e1) (desugar e2))]
    [setS (v a) (setC v (desugar a))]
    [objS (ns vs) (objC ns (map desugar vs))]
    [msgS (o m a) (appC (msgC (desugar o) m) (desugar a))]
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
    [objC (ns vs) (objV ns
                        (map (lambda (e) (interp e env)) vs))]
    [msgC (o m) (lookup-msg m (interp o env))]
    ))

(define (lookup-msg [msg : symbol] [obj : Value]) : Value
  (local ((define (h los lov)
            (cond [(empty? los) (error 'lookup-msg "msg not found")]
                  [(symbol=? msg (first los)) (first lov)]
                  [else (h (rest los) (rest lov))])))
          (h (objV-names obj) (objV-vals obj))))
          
  

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

;; ((lambda (x) (* x x)) 3)
;; (lambda (x) x)

#|
(test (AS-swf '(zero 5)) 0)
(test (AS-swf '(sqr 5)) 25)
(test (AS-swf '(to-fourth (+ 3 -1))) 16)
(test/exn (AS-swf '(+ 3 y)) "lookup: encountered free variable")
(test/exn (AS-swf '(fvf 3)) "lookup: encountered free variable")
(test/exn (AS-swf '(af 3)) "lookup: encountered free variable")
(with (t (btree 3 (btree 1 (btree 0 empty empty) (btree 2 empty empty))
                (btree 4 empty empty)))
      (rec (find (lambda x t)
                 (cond [(empty? t) false]
                       [(= (btree-data t) x) true]
                       [else (or (find x (btree-left t))
                                 (find x (btree-right t)))])
                 (find 4 t))))
|#

