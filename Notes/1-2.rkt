#lang racket
;;; Notes

;;; (cons 1 '(2 3)) --> '(1 2 3)
;;; (list 1 2 3) --> '(1 2 3)
;;; '(1 2 3) --> '(1 2 3)

;;; (define L '(1 2 3))
;;; (first L) --> 1
;;; (rest L) --> '(2 3)
;;; (last L) --> 3

;;; Higher Order Functions
;;; (sort List ineq) --> List: sorts the list based on the inequality
;;; (sort '(4 3 2 1) <) --> '(1 2 3 4)

;;; (map func List) --> List: applies the function to every element in the list 
;;; (map sqr '(1 2 3)) --> '(1 4 9)

;;; (append L1 L2) --> '(L1 L2): returns the list with L2 appended to L1
;;; (append '(1 2) '(3 4)) --> '(1 2 3 4)






