#lang racket
(require racket/trace)

;;; (define (rev l)
;;;     (if (null? l)
;;;         null
;;;         (append (last l) (rev (rest l)))
;;;     )
;;; )

(define (revt x y)
    (if (null? x)
        y
        (revt (rest x) (cons (first x) y))
    )
)

(define (rev l)
    (revt l '())
)

(define x 2)

(define (sumf f n)
        (if (= n 0)
            0
            (+ (f n) (sumf f (- n 1)))))

(define (nth-power n) (lambda (x) (power x n)))

(define (power x n) 
    (expt x n)
)

(define (make-adder x) (lambda (y) (+ x y)))
    
(define add1 (make-adder 1))

(trace add1)

(define (charfun pred?) 
    (lambda (x) (if (pred? x) 1 0)))