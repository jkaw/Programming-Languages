#lang plai-typed
;;desugar to parse to interp
(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (s : symbol)]
  [appC (fn : symbol) (arg : (listof ExprC))]
  [ifC (test : ExprC) (t : ExprC) (f : ExprC)]
  [withC (name : symbol) (svalue : ExprC) (body : ExprC)])

(define-type ExprS
  [numS (n : number)]
  [plusS (l : (listof ExprS))]
  [multS (l : (listof ExprS))]
  [uminusS (e : ExprS)]
  [idS (s : symbol)]
  [appS (fun : symbol) (arg : (listof ExprS))]
  [ifS (test : ExprS) (t : ExprS) (f : ExprS)]
  [withS (name : (listof symbol)) (svaluesc : (listof ExprS)) (body : ExprS)])

(define-type fundefC
  [fundef (name : symbol) (param : (listof symbol)) (body : ExprC)])


  
  
  (define (parse [s : s-expression]) : ExprS
    (cond [(s-exp-number? s) (numS (s-exp->number s))]
          [(s-exp-list? s)
           (let ([sl (s-exp->list s)])
             (case (s-exp->symbol (first sl))
               [(+) (cond
                      [(< (length sl) 2) (error 'parse "Please input two or more numbers")]
                      [else (plusS (map parse (rest sl)))])]
               [(*) (cond
                      [(< (length sl) 2) (error 'parse "Please input two or more numbers")]
                      [else (multS (map parse (rest sl)))])]
               [(if) (ifS (parse (second sl))
                          (parse (third sl))
                          (parse (fourth sl)))]
               [(with)
                (let* ([idbind-prs (map s-exp->list (s-exp->list (second sl)))]
                       [ids (map s-exp->symbol (map first idbind-prs))]
                       [binds (map parse (map second idbind-prs))])
                  (withS ids binds (parse (third sl))))]
               [else (cond 
                       [(= (length sl) 2) (appS (s-exp->symbol (first sl))
                                                (map parse (rest sl)))]
                       [else (error 'parse "function name in application must be a symbol")])]))]
          [else (error 'parse "invalid input")]))
  
  (define (get-fundef [fname : symbol] [defs : (listof fundefC)]) : fundefC
    (cond [(empty? defs) (error 'get-fundef (string-append "undefined function " (symbol->string fname)))]
          [else (if (symbol=? fname (fundef-name (first defs))) (first defs)
                    (get-fundef fname (rest defs)))]))
  
  
  
  
  (define (interp [e : ExprC] [funs : (listof fundefC)]) : number
    (type-case ExprC e
      [numC (n) n]
      [plusC (l r) (+ (interp l funs) (interp r funs))]
      [multC (l r) (* (interp l funs) (interp r funs))]
      [idC (s) (error 'interp "no free variable")]
      [appC (f a) (let ([the-function(get-fundef f funs)]
                        [argvs (map (lambda (a) (numC (interp a funs))) a)])
                   (interp (subhelp a
                                     (fundef-param the-function)
                                     (fundef-body the-function)                                
                                     ) funs)
                    
                    )]
      [ifC (test t f) 
           (local ([define test-result (interp test funs)])
             (if (zero? test-result)
                 (interp f funs)
                 (interp t funs)))]
      [withC (i bv b)
             (interp (subst (numC (interp bv funs)) i b) funs)]
             ))
  
  (define (desugar [e : ExprS]) : ExprC
    (type-case ExprS e
      [numS (n) (numC n)]
      [plusS (l) (foldr (lambda (x y) (plusC (desugar x) y)) (numC 0) l)]
      [multS (l) (foldr (lambda (x y) (multC (desugar x) y)) (numC 1) l)]
      [uminusS (e) (multC (numC -1) (desugar e))]
      [idS (s) (idC s)]
      [appS (f es) (appC f (map desugar es))]
      [ifS (test t f) (ifC (desugar test)
                           (desugar t)
                           (desugar f))]
      [withS (n v b)
             (if (empty? n)
                 (desugar b)
                 (withC (first n) (desugar (first v))
                        (desugar (withS (rest n) (rest v) b))))]))
  
  
  (define (subhelp [what : (listof ExprC)] [for : (listof symbol)] [in : ExprC]) : ExprC
    (cond
      [(empty? what) in]
      [else (subhelp (rest what) (rest for) (subst (first what) (first for) in))]))
  
  (define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
    (type-case ExprC in
      [numC (n) in]
      [idC (s) (if (symbol=? s for) what in)]
      [appC (f a) (appC f (map (lambda (x) (subst what for x)) a))]
      [plusC (l r) (plusC (subst what for l) (subst what for r))]
      [multC (l r) (multC (subst what for l) (subst what for r))]
      [ifC (test t f) (ifC (subst what for test)
                           (subst what for t)
                           (subst what for f))]
      [withC (n v b)
             (if (symbol=? n for)
                 in
                 (withC n (subst v for in) (subst b for in)))]))

  
  
  
  (define (AS-swf s funs)
    (interp (desugar (parse s)) funs))


  (test (AS-swf (number->s-exp 5) (list)) 5)
  