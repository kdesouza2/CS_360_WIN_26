#lang racket

;;; Lab 2

;;; Q1

;;; The following function recursively computes the powerset of a set. We showed by induction
;;; that the powerset of a set of size n has size 2 n . Note that the function powerSet computes
;;; the (powerSet (rest S)) twice. Modify the code to use let so that it is only computed once.
;;; Write down a recurrence relation for the computing time of powerSet as a function of the
;;; size of S. What is the asymptotic computing time of the original and modified versions of
;;; powerSet

;;; (define (powerSet S)
;;;     (if (null? S)
;;;         '(())
;;;         (append (powerSet (rest S)) (map (lambda (S1) (cons (first S) S1)) (powerSet (rest S))))
;;;     )
;;; )

(define (powerSet S)
    (if (null? S)
        '(())
        (let ([ps (powerSet (rest S))])
            (append ps (map (lambda (S1) (cons (first S) S1)) ps))
        )
    )
)
