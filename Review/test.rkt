#lang racket
(require racket/trace)

;;; Question 1 — Higher-Order Functions

;;; Problem:
;;; Write a function myfilter that takes a list and a predicate function and returns a list of elements that satisfy the predicate.

;;; Example:

;;; (myfilter even? '(1 2 3 4 5))
; => '(2 4)

;;; Hint: Use filter or map and a conditional.

(define (myfilter pred? L)
    (filter pred? L)
)

;;; Question 2 — Tail Recursion

;;; Problem:
;;; Write a tail-recursive function sum-list that sums all the elements of a list.

;;; Example:

;;; (sum-list '(1 2 3 4))
;;; ; => 10

(define (sum-list l)
    (sum-help l)
)

(define (sum-help l)
    (if (null? l)
        0
        (+ (first l) (sum-help (rest l)))
    )
)

;;; Question 3 — Substitution Model of Evaluation

;;; Problem:
;;; Evaluate this step by step using the substitution model:

;;; (define (apply-twice f x)
;;;   (f (f x)))


;;; (apply-twice (lambda (y) (* y 2)) 3)

;;; Question: Show each substitution until you get the final value.



;;; Question 5 — Streams

;;; Problem:
;;; Create a stream of all even numbers from 2 to 20, then sum them using stream-fold.

;;; ; expected result: 2 + 4 + 6 + ... + 20 = 110

;;; Hint: Use stream-enumerate-interval, stream-map, and stream-fold.

(define (stream-enumerate-interval a b)
    (if (> a b)
        empty-stream
        (stream-cons a (stream-enumerate-interval (+ a 2) b))
    )
)

(stream-fold + 0 (stream-enumerate-interval 2 20))
