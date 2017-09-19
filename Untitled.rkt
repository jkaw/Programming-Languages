#lang plai-typed

(define-type-alias label number)

(define table (make-hash empty))
#|
(define (read-number/suspend [prompt : string] rest)
  (let ([g (new-label)])
    (begin (hash-set! table g rest)
           (display prompt)
           (display "to respond, resume with input and lable ") (display g)
           (error 'stop "until client sends response"))))
|#

#|(define (read-number/stateless [prompt : string] rest)
  (let ([g (new-label)])
    (begin (hash-set! table g rest)
           (display prompt)
           (display "to respond, resume with input and lable ") (display g)
           (error 'stop "until client sends response"))))

(define (resume g n)
  ((some-v (hash-ref table g)) n))
|#

#|(display
 (+ (read-number/suspend "First number ")
    (read-number/suspend "Second number ")))
|#
(define new-label
  (let ([state-var -1])
    (lambda ()
    (begin (set! state-var (+ 1 state-var))
           state-var))))
#|
(read-number/suspend
 "First number "
 (lambda (v1)
   (read-number/suspend
    "Second number "
    (lambda (v2)
      (display (+ v1 v2))))))
|#
#|
(define cookie -1)


(define (num2 v2)
  (display (+ cookie v2)))
(define (num1 v1)
  (begin (set! cookie v1)
  (read-number/stateless "Enter second " num2)))
(read-number/stateless "Enter first " num1)
|#


;; #' is read as syntax. Saying this next part is syntax.
(define-syntax (cps e)
  (syntax-case e (with rec lam cnd seq set quote display read-number)
    ;; with
    [(_ (with (v e) b))
     #'(cps ((lam (v) b) e))]
    ;; rec
    [(_ (rec (f fv) b))
     #'(cps (with (f (lam (x) (error 'rec "better not encounter this")))
                  (seq (set f fv)
                       b)))]
    ;; lam
    ;; cnd
    ;; display
    ;; read-number
    ;; seq
    [(_ (seq e1 e2))
     #'(lambda (k)
         ((cps e1) (lambda (e1v)
                     ((cps e2) k))))]
    ;; set
    [(_ (set v e))
     (identifier? #'v)
     #'(lambda (k)
         ((cps 3) (lambda (ev)
                    (k set! v ev))))]
    ;; quote
    [(_ 'e)
    #'(lambda (k)
        (k 'e))]
    ;; app-1
    ;; app-2
    ;; atomic
    [(_ atomic)
     #'(lambda (k)
         (k atomic))]
     
    ))















