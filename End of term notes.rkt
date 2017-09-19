#lang plai-typed
;; 2 weeks to go

(define-type Value
  [numV (n : number)]
  [closV (param : symbol) (body : TyExprC) (env : Env)])

(define-type Type
  [numT]
  [funT (argT : Type) (retT : Type)]
  )

;; Our internal representaion of expressions in the core language
(define-type TyExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fn : TyExprC) (arg : TyExprC)]
  [plusC (l : TyExprC) (r : TyExprC)]
  [multC (l : TyExprC) (r : TyExprC)]
  [ifC (cond : TyExprC) (then : TyExprC) (else : TyExprC)]
  [lamC (param : symbol) (argT : Type) (retT : Type) (body : TyExprC)]
  )




(define (type-check [expr : TyExprC] [tenv : TyEnv]) : Type
  (type-case TyExprC expr
    [numC (n) (numT)]
    [idC (s) (tlookup s tenv)]
    [appC (f a) (let ([ft (type-check f tenv)]
                      (at (type-check a tenv)))
                  (cond [(not (funT? ft)) (error 'type-check "attempt to apply non-function")]
                        [(not (equal? at (funT-argT ft)))
                         (error 'type-check "input type mismatch")]
                        [else (funT-retT ft)]))]
    [plusC (l r) (let ([lt (type-check l tenv)]
                       [rt (type-check r tenv)])
                   (cond [(and (numT? lt) (numT? rt)) numT]
                         [else (error 'type-check "+: at least one operand not a number")]))]
    [multC (l r) (let ([lt (type-check l tenv)]
                       [rt (type-check r tenv)])
                   (cond [(and (numT? lt) (numT? rt)) numT]
                         [else (error 'type-check "*: at least one operand not a number")]))]
    [ifC (c t e) (let ([ct (type-check c tenv)]
                       [tt (type-check t tenv)]
                       [et (type-check e tenv)])
                   (cond [(and (numT? ct) (equal? tt et)) tt]
                         [else (error 'type-check "if: some error occurred")]))]
    [lamC (p argT retT b) (let ([bt (type-check b (extend-tenv (tbind p argT) tenv))])
                            (cond [(equal? bt retT)
                                   (funT argT retT)]
                                  [else (error 'type-check "lambda: return annotation does not match body")]))]
    ))

(define-type-alias TyEnv (listof tbind))
(define-type TBind
  [tbind (s : symbol) (st : Type)])
(define (extend-tenv [s : symbol] [st : Type] [tenv : TyEnv]) : TyEnv
  (cons (tbind s st) tenv))
(define (tlookup [s : symbol] [tenv : TyEnv]) : Type
  (cond [(empty? tenv) (error 'tlookup "Unbound identifier")]
        [(symbol=? s (tbind-s (first tenv))) (tbind-st (first tenv))]
        [else (tlookup s (rest tenv))]))

;(AS-swf '((lambda (x) (+ x x)) 3))
;; (test (AS-swf '(with (fact (lambda [n : num] : num (if n (* n (fact (- n 1))) 1)))
;;                      (fact 5)))
;;      120)



