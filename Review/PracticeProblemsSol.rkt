#lang racket

; Question 1 [Higher order functions

(define (myzip x y)
  (map cons x y))

(myzip '(1 2 3) '(a b c))
; '((1 . a) (2 . b) (3 . c))

;  Question 2 [tail recursion]

(define (revt x y)
  (if (null? x) y (revt (rest x) (cons (first x) y))))

(define (myrev x) (revt x null))

(myrev '(1 2 3))

; '(3 2 1)

; Question 3 [substitution model of evaluation

(define (sumf f n)
  (if (= n 0)
      0
      (+ (f n) (sumf f (- n 1)))))

 (define (nth-power n)
  (lambda (x)
    (power x n)))

(define (power a n)
 (if (= n 0) 1 (* a (power a (- n 1)))))

(sumf (nth-power 3) 2) ; [premise]
(sumf (lambda (x) (power x 3)) 2) ; [def nth-power]
(if (= 2 0) 0 (+ ((lambda (x) (power x 3)) 2) (sumf (lambda (x) (power x 3)) (- 2 1)))) ; [def sumf]
(if #f 0 (+ ((lambda (x) (power x 3)) 2) (sumf (lambda (x) (power x 3)) (- 2 1)))) ; [eval =]
(+ ((lambda (x) (power x 3)) 2) (sumf (lambda (x) (power x 3)) (- 2 1))) ; [=eval if]
(+ (power 2 3) (sumf (lambda (x) (power x 3)) (- 2 1)))  ; [def lambda]                 
(+ 8 (sumf (lambda (x) (power x 3)) (- 2 1)))            ; [eval power]
(+ 8 (sumf (lambda (x) (power x 3)) 1))                  ; [eval -]
(+ 8 (if (= 1 0) 0 (+ ((lambda (x) (power x 3)) 1) (sumf (lambda (x) (power x 3)) (- 1 1))))) ; [def sumf]
(+ 8 (if #f 0 (+ ((lambda (x) (power x 3)) 1) (sumf (lambda (x) (power x 3)) (- 1 1)))))  ; [eval =]
(+ 8 (+ ((lambda (x) (power x 3)) 1) (sumf (lambda (x) (power x 3)) (- 1 1)))) ; [eval if]
(+ 8 (+ (power 1 3) (sumf (lambda (x) (power x 3)) (- 1 1)))) ; [def lambda]
(+ 8 (+ 1 (sumf (lambda (x) (power x 3)) (- 1 1))))         ;[eval power]
(+ 8 (+ 1 (sumf (lambda (x) (power x 3)) 0)))               ; [eval -]
(+ 8 (+ 1 (if (= 0 0) 0 (+ ((lambda (x) (power x 3)) 0))))) ; [def sumf]
(+ 8 (+ 1 (if #t 0 (+ ((lambda (x) (power x 3)) 0)))))      ; [eval =]
(+ 8 (+ 1 0))                                               ; [eval if]
9                                                           ; arithmetic

; (sumf (nth-power 3) 2)
;
; sumf binds n = 2 so with dynamic scope when calling the function
; returned by nth-power n = 2 and (sumf (nth-power 3) 2)
; returns 0+1+4 = 5 instead of 9.
; Verified in dynamic scope version of mceval.

; Question 4 [Environment model of evaluation
; In this question there are a sequence of expressions,
; and you are to show their effect on various environments.  
(define (make-adder x)
  (lambda (y) (+ x y)))

(define add1 (make-adder 1))
(define x 2)
(add1 2)

; Global Environment ((make-adder:proc add1:proc x:2))
; Environment stored with add1 ( ((x:1)) Global-Environment)
; Environment after add1 called ( ((y:2)) ((x:1)) Global-Environment)

; In dynamic scope, there is no environment stored with add1.
; The global environment is used when add1 is called.
; Thus x is bound to 2 and (add1 2) returns 4.

; Question 5 [streams]

(require racket/stream)
(require racket/trace)
(require math/number-theory)

(define (charfun pred?) 
          (lambda (x) (if (pred? x) 1 0)))

(define (stream-enumerate-interval a b)
  (if (> a b) empty-stream
      (stream-cons a (stream-enumerate-interval (+ a 1) b))))

(define a 2)
(define b 100)
(stream-fold + 0 (stream-map (charfun prime?) (stream-enumerate-interval a b)))

; Question 6 [applicative order, normal order and memoization]
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))

(f 5)

;Applicative order - also called call by value
;  5 arithmetic operations
; (f 5) => (sum-of-squares (+ 5 1) (* 5 2)) [evaluate operands before applying function]
;       => (sum-of-squares 6 10)
;       => (+ (square 6) (square 10))
;       => (+ (* 6 6) (* 10 10)) => (+ 36 100) => 136
;
; Normal order - also called call by name [delay evaluation of arguments]
; 7 arithmetic operataions which is 2 more than applicative order
; (f 5) => (sum-of-squares (+ 5 1) (* 5 2))
;       => (+ (square (+ 5 1)) (square (* 5 2)))
;       => (+ (* (+ 5 1) (+ 5 1))) (* (* 5 2) (* 5 2)))
;       => (+ (* 6 6) (* 10 10)) => (+ 36 100) => 136
;
; memoization - also called call by need
; Avoid computing (+ 5 1) andd (* 5 2) twice using memoization
; thus reducing arithmetic operations to 5 which is same as applicative order

