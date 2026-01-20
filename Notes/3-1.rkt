#lang racket

(define (list? L)
    (cond
        [(null? L) #t]
        [(cons? L) (list? (cdr L))]
        [else #f]
    )

)

;;; How to Prove Termination
;;;     For recursive functions show that the “size” of the inputs get smaller and eventually must hit a base case
;;;     Size is defined to be a function to the natural numbers
;;;     Use the well ordering principle of the natural numbers to conclude that the number of recursive 
;;;         calls can not be infinite


;;; Equational Reasoning
;;;     Prove equivalence of racket expressions by repeatedly replacing subexpressions by equivalent
;;;     subexpressions until the two expressions are equal

;;; Axioms
;;; 1. (first (cons x y)) ≡ x [first Axiom]
;;; 2. (rest (cons x y)) ≡ y [rest Axiom]
;;;     • (cons (first x) (rest x)) ≡ x [cons Axiom]
;;; 3. (cons? (cons x y)) ≡ #t [cons? Axiom]
;;;     • Otherwise #f
;;; 4. (null? null) ≡ #t [null? Axiom]
;;;     • Otherwise #f
;;; 5a. x = #f ⇒ (if x y z) ≡ z [if Axiom]
;;; 5b. x ≠ #f ⇒ (if x y z) ≡ y

;;; Structural Induction
;;;     When using induction on recursively defined data structures like lists you can induct on the 
;;;         size of the data structure = to the number of calls to the constructors.
;;;     When trying to show a property for a data structure of a given size, you can assume that the 
;;;         property holds when making a recursive call on a smaller data structure. You must make sure that 
;;;         the property holds for all constructors including base cases.
;;;     With lists (rest …) will return a smaller data structure (at least one fewer cons)
;;;     Structural induction allows you to induct on the recursive data structure without being explicit about the size
;;;         provided the IH is applied to smaller objects.

(define (length L)
    (if (null? L)
        0
        (+ 1 (length (rest L)))
    )
)

;;; Properties
;;; 1. (length null) = 0
;;; 2. (length (cons x y)) = (+ 1 (length y))

;;; In Class Exercise 

;;; Q1: Prove properties 1 and 2 of length using equational reasoning
;;; Property 1: Prove (length null) = 0 → (length L)

;;; L = null
;;; LHS
;;; = (if (null? null) 0 (+ 1 (length (rest null)))) [by def of length]
;;; = (if #t 0 (+ 1 (length (rest null)))) [null? axiom]
;;; = 0
;;; RHS
;;; = 0
;;; LHS = RHS 

;;; Property 2: Prove (length (cons x y)) = (+ 1 (length y)) → (length L)

;;; 

(define (reverse L)
    (if (null? L)
        null
        (append (reverse (rest L)) (cons (first L) null))
    )
)

;;; Properties
;;; 1. (list? l) → (list? (reverse l))
;;; 2. (length (reverse x)) = (length x)
;;; 3. (reverse (append x y)) = (append (reverse y) (reverse x))
;;; 4. (reverse (reverse x)) = x
;;; 5. Let L = (L1 … Ln ) and R = (reverse L).
;;;     n > 0 →. Ri = Ln+1-i

;;; Q2: Prove properties 1 and 2 of reverse using equational reasoning. Assume append properties
;;; Property 1: Prove (list? l) → (list? (reverse l))

;;; Base Case: l = null
;;; (list? null) = (list? (reverse null))

;;; IH: (list? k) --> (list? (reverse k))

;;; IC: (list? (rest k)) --> (list? (reverse (rest k)))
;;; RHS:
;;; (list? (if (null? (rest k)) null (append (reverse (rest k)) (cons (first (rest k) null)))))





(define (append x y)
    (if (null? x)
        y
        (cons (first x) (append (rest x) y))
    )
)

;;; Properties
;;; 1. (and (list? x) (list? y)) → (list? (append x y))
;;; 2. (append null y) = y
;;; 3. x ≠ null → (first (append x y)) = (first x)
;;; 4. (append x null) = x
;;; 5. (length (append x y)) = (+ (length x) (length y))
;;; 6. (append x (append y z)) = (append (append x y) z)