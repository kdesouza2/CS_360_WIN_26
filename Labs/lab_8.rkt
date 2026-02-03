#lang racket

;;; 1. Study the code in lazymceval.rkt. Trace through, by hand, what happens when the following
;;; expressions are evaluated. What would happen if this was run with the original (applicative
;;; order) interpreter was used?

;;; (define (try a b)
;;;     (if (= a 0) 1 b)
;;; )

;;; (try 0 (/ 1 0)