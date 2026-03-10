#lang racket

(define mycons
  (lambda (x y)
    (lambda (f)
      (f x y))))
;Value: mycons

(define mycar
  (lambda (f)
    (f (lambda (x y) x))))
;Value: mycar

(define mycdr
  (lambda (f)
    (f (lambda (x y) y))))
;Value: mycdr

(mycar (mycons 'a 'b))
;Value: a


(mycdr (mycons 'a 'b))
;Value: b

; Now that we have the ability to create pairs in the lambda calculus,
; there is an alternative approach to implementing the predecessor function.
;
; The idea is to apply the n-fold composition of the function, that
; takes the pair (m-1,m) and produces the pair (m,m+1), to the pair (0,0)
; This produces the pair (n-1,n) and now take the first element of that pair.

(define zero 
  (lambda (f)
    (lambda (x) x)))
;Value: zero

(define one
  (lambda (f)
    (lambda (x) (f x))))
;Value: one

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))
;Value: two

(define three
  (lambda (f)
    (lambda (x) (f (f (f x))))))
;Value three

;
; successor function:  (succ n) => n+1
;

(define succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
	(f ((n f) x))))))

(define (add1 n) (+ n 1))

(define (pred n)
  (mycar ((n (lambda (p) (mycons (mycdr p) (succ (mycdr p)))))
     (mycons zero zero))))

(((pred three) add1) 0)




