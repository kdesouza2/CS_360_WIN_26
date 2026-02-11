#lang racket

;;; Question 1 — Basic Stream Construction

;;; Problem:

;;; Create a stream of numbers from 1 to 10, then convert it to a list.

;;; Hint: Use stream-enumerate-interval.

;;; Expected Result:

;;; '(1 2 3 4 5 6 7 8 9 10)

(define (stream-enumerate-interval a b)
    (if (> a b)
        empty-stream
        (stream-cons a (stream-enumerate-interval (+ a 1) b))
    )
)

;;; Question 2 — Stream Mapping

;;; Problem:

;;; Create a stream of numbers from 1 to 8, then square every number.

;;; Expected Result:

;;; '(1 4 9 16 25 36 49 64)

(define (stream-square s)
    (stream-map (lambda (x) (* x x)) s)
)

;;; Question 3 — Stream Filtering

;;; Problem:

;;; Create a stream from 1 to 20, then keep only the odd numbers.

;;; Expected Result:

;;; '(1 3 5 7 9 11 13 15 17 19)

(define (stream-odd s)
    (stream-filter odd? s)
)

;;; Question 4 — Stream Folding (Very Common Exam Question)

;;; Problem:

;;; Sum all numbers from 1 to 100 using streams.

;;; Hint: Use stream-fold.

;;; Expected Result:

;;; 5050


(define (stream-sum s)
    (stream-fold + 0 s)
)

;;; Question 5 — Combining map + filter

;;; Problem:

;;; From numbers 1 to 20:

;;; ✔ Keep only even numbers
;;; ✔ Square them
;;; ✔ Sum them

(define (stream-comb s)
    (stream-fold + 0 (stream-map (lambda (x) (* x x)) (stream-filter even? s)))
)

;;; Question 6 — Infinite Stream (EXAM GOLD)

;;; Problem:

;;; Define an infinite stream of all natural numbers starting at 1.

;;; Hint: Self-referential stream.

;;; ✔ First few values:

;;; 1 2 3 4 5 ...

(define (stream-naturals)
    (stream-cons 1 (stream-map add1 stream-naturals))
)