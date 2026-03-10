#lang racket
(define zero 
  (lambda (f)
    (lambda (x) x)))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define three
  (lambda (f)
    (lambda (x) (f (f (f x))))))

;Show by repeated substitution that (succ two) is equal to three where

(define succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
	(f ((n f) x))))))

(define E0 (succ two) )
(define E1 (lambda (f)(lambda (x) (f ((two f) x)))) ) ; def of succ
(define E2 (lambda (f)(lambda (x)
                        (f (((lambda (f) (lambda (x) (f (f x)))) f) x)))) ) ;def of two
(define E3 (lambda (f)(lambda (x) (f ((lambda (x) (f (f x))) x)))) ) ; apply two to f
(define E4 (lambda (f)(lambda (x) (f (f (f x))))) ) ; apply two with f fixed to x
(define E5 three)  ; def of three

(define (add1 x) (+ x 1))

((E0 add1) 0)
((E1 add1) 0)
((E2 add1) 0)
((E3 add1) 0)
((E4 add1) 0)
((E5 add1) 0)
