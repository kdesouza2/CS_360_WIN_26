#lang racket

;;; Arithmetic Expressions
;;; ArithExp := Constant | Variable | (Plus ArithExp ArithExp) | (Mult ArithExp ArithExp)

(define (arith-expr? expr)
    (cond
        [ (constant? expr) #t ]
        [ (variable? expr) #t ]
        [ (plus? expr) (and (arith-expr? (op1 expr)) (arith-expr? (op2 expr))) ]
        [ (mult? expr) (and (arith-expr? (op1 expr)) (arith-expr? (op2 expr))) ]
        [else #f ]
    )
)

;;; Predicate
;;; Constructors
;;;     make-plus, make-mult
;;; Accessors
;;; op1, op2
;;; Termination
;;; Recursion template
;;; Induction template

;;; Differentiation

(define (diff expr var)
    (cond
        [(constant? expr) 0]
        [(variable? expr) (if (equal? expr var) 1 0)]
        [(plus? expr) (make-plus (diff (op1 expr) var)
        (diff (op2 expr) var))]
        [(mult? expr) (make-plus
        (make-mult (op1 expr) (diff (op2 expr) var))
        (make-mult (diff (op1 expr) var) (op2 expr)))]
    )
)

;;; Simplification of Plus

(define (plus-simp expr1 expr2)
    (cond
        [ (and (constant? expr1) (constant? expr2)) (+ expr1 expr2) ]
        [ (equal? expr1 0) expr2 ]
        [ (equal? expr2 0) expr1 ]
        [ (make-plus expr1 expr2) ]
    )
)

;;; Simplification of Mult

(define (mult-simp expr1 expr2)
    (cond
        [ (and (constant? expr1) (constant? expr2)) (* expr1 expr2) ]
        [ (equal? expr1 0) 0 ]
        [ (equal? expr2 0) 0 ]
        [ (equal? expr1 1) expr2 ]
        [ (equal? expr2 1) expr1 ]
        [ else (make-mult expr1 expr2) ]
    )
)

;;; Recursive Simplification

(define (arith-simp expr)
    (cond
        [ (constant? expr) expr ]
        [ (variable? expr) expr ]
        [ (plus? expr) (let ([simpexpr1 (arith-simp (op1 expr))]
            [simpexpr2 (arith-simp (op2 expr))])
            (plus-simp simpexpr1 simpexpr2)) ]
        [ (mult? expr) (let ([simpexpr1 (arith-simp (op1 expr))]
            [simpexpr2 (arith-simp (op2 expr))])
            (mult-simp simpexpr1 simpexpr2)) ]
    )
)
