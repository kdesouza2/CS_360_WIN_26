#lang racket
;
; Define boolean constants as functions
;

(define true (lambda (x y) x))
;Value: true

(define false (lambda (x y) y))
;Value: false

; 
; The following can be used for debugging.
;

(true #t #f)
;Value: #t

(false #t #f)
;Value: #f


;
; define and test boolean connectives and, or, not.
;

(define and (lambda (p q) (p q false)))
;Value: and

((and true true) #t #f)
;Value: #t

((and true false) #t #f)
;Value: #f

((and false true) #t #f)
;Value: #f

((and false false) #t #f)
;Value: #f

(define or (lambda (p q) (p true q)))
;Value: or

((or false false) #t #f)
;Value: #f

((or false true) #t #f)
;Value: #t

((or true false) #t #f)
;Value: #t

((or true true) #t #f)
;Value: #t


(define not (lambda (p) (p false true)))
;Value: not

((not true) #t #f)
;Value: #f

((not false) #t #f)
;Value: #t

;
; a predicate is a function that returns true or false.
; 

(define zero (lambda (f) (lambda (x) x)))
;Value: zero

(define one (lambda (f) (lambda (x) (f x))))
;Value: one

(define two (lambda (f) (lambda (x) (f (f x)))))
;Value: two

(define three
  (lambda (f)
    (lambda (x) (f (f (f x))))))
;Value: three


(define iszero (lambda (n) ((n (lambda (x) false)) true)))
;Value: iszero

((iszero zero) #t #f)
;Value: #t

((iszero one) #t #f)
;Value: #f

((iszero two) #t #f)
;Value: #f

; 
; once we have predicates, we can implement if-then-else statements
;

(define if-then-else (lambda (p x y) (p x y)))
;Value: if-then-else

(if-then-else (iszero zero) 0 1)
;Value: 0

(if-then-else (iszero one) 0 1)
;Value: 1

(define addc
  (lambda (m n)
    (lambda (f)
      (lambda (x)
	((m f) ((n f) x))))))

; Another implementation of the predecessor function.
; PRED := λn.n (λg.λk.ISZERO (g 1) k (PLUS (g k) 1)) (λv.0) 0
; This implementation utilizes the iszero predicate and an
; implicit if-then-else.  The function
; (λg.λk.ISZERO (g 1) k (PLUS (g k) 1)) is applied n times.
; This adds 1 each time to zero except the first time so
; that n-1 is returned.  The resulting function λk.k+(n-1)
; is applied to zero returning n-1.

(define pred
  (lambda (n)
    (((n (lambda (g) (lambda (k) ((iszero (g one)) k (addc (g k) one)))))
      (lambda (v) zero)) zero)))

(((pred three) add1) 0)











