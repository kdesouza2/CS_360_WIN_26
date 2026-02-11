#lang racket

;;; 1. Study the code in lazymceval.rkt. Trace through, by hand, what happens when the following
;;; expressions are evaluated. What would happen if this was run with the original (applicative
;;; order) interpreter was used?

(define (try a b)
    (if (= a 0) 1 b)
)

(try 0 (/ 1 0))

;;; When we run it in applicative order, it evaluates the arguments before it goes into the body. Therefore
;;; with the original it would have an error because you can't divide by 0. However. with lazymceval it will just 
;;; return 1 because it falls into (= a 0) and therefore doesn't evaluate the b

;;; 2. What happens when we call (try 1 (/ 2 1))? Note that you can use the value function to
;;; evaluate a thunk. What happens when you call this function twice with the memoized evaluator
;;; in lazymemmceval.rkt?

