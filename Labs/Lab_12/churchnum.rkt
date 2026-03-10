#lang racket
;
; Arithmetic in Lambda Calculus
;

;
; Encode the natural number n as the n-fold composition operator
; (lambda (f)
;  (lambda (x) (f (f ... (f x)...))))
; Such an encoding is called a Church numeral.
; As we will see, it is possible to perform arithmetic with Church
; numerals.


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
;Value: three

;
;  Using the function add1, we can verify that the encoding is
;  correct.  I.E.  ((n add1) 0) => n
;

(define add1
  (lambda (x) (+ x 1)))
;Value: add1

((one add1) 0)
;Value: 1

((two add1) 0)
;Value: 2

;
; successor function:  (succ n) => n+1
;

(define succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
	(f ((n f) x))))))
;Value: succ

(define twop (succ one))
;Value: twop

((twop add1) 0)
;Value: 2

(((succ two) add1) 0)
;Value: 3

;
; addition done two different ways:
; one using by applying n compositions folled by m compositions and
; the other using the successor function.
;

(define addc
  (lambda (m n)
    (lambda (f)
      (lambda (x)
	((m f) ((n f) x))))))
;Value: addc

(define four (addc two two))
;Value: four

((four add1) 0)
;Value: 4


(define addc2
  (lambda (m n)
    ((m succ) n)))
;Value: addc2


(define twos (addc2 two two))
;Value: twos

((twos add1) 0)
;Value: 4

;
; multiplication done two different ways:
; one using iterated composition and the other using add.
;

(define mulc
  (lambda (m n)
    (lambda (f)
      (lambda (x)
	((m (n f)) x)))))
;Value: mulc

(define fourm1 (mulc two two))
;Value: fourm1

((fourm1 add1) 0)
;Value: 4

(define mulc2
  (lambda (m n)
    ((m (lambda (a) (addc n a))) zero)))
;Value: mulc2

(define fourm2 (mulc2 two two))
;Value: fourm2

((fourm2 add1) 0)
;Value: 4


; Subtraction and division are much harder.  The necessary
; piece is the predecessor function (pred n) -> n-1 for n > 0.
; This is tricky.  The idea is to take the n-fold composition
; of a function f with a function that composes a new function h
; with the n-1 fold composition of f.  Then evaluate h on the
; identity function.

; PRED := λnfx.n (λgh.h (g f)) (λu.x) (λu.u)
; The n fold composition of (λgh.h (g f)) applied to (λu.x)
; gives the composition of h with the (n-1) fold composition of f.
; (λh.h ((n-1) f)) and applying this to the identity function (λu.u)
; gives λfx.((n-1) f).

(define pred (lambda (n)
               (lambda (f)
                 (lambda (x)
                   (((n (lambda (g) (lambda (h) (h (g f)))))
                      (lambda (u) x))
                      (lambda (u) u))))))

(((pred zero) add1) 0)
; 0

(((pred one) add1) 0)
; 0

(((pred two) add1) 0)
; 1

(((pred three) add1) 0)
;2







