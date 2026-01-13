#lang racket


;;; Lab 1

;;; Q 1
;;; Implement the function (lookup var env) where var is a variable, i.e. a symbol, and env is an
;;; environment. An environment is a list of bindings and a binding a list of length two where the
;;; first element is a variable and the second is the value bound to the variable. The function
;;; lookup returns the value bound to a given variable in the specified environment. If the variable
;;; is not found in the environment, then use the Racket function error to throw an error. E.G.
;;; (lookup ‘y ‘((x 2) (y 3))) returns 3.

(define (lookup var env)
    (cond 
    [(null? env) (error "Variable not present in environment!")]
    [(equal? var (first (first env))) (first (rest (first env)))]
    [else (lookup var (rest env))]
    )
)

;;; Q 2
;;; The following function is given a list and returns a new list whose elements are those in the
;;; input but in reverse order. E.G. (rev ‘(1 2 3)) = ‘(3 2 1)
;;; (define (rev l)
;;;     (if (null? l)
;;;         null
;;;         (append (rev (rest l)) (cons (first l) null))))
;;; The function rev uses the function append which appends the list y after the list x.
;;; (define (append x y)
;;;     (if (null? x)
;;;         y
;;;         (cons (first x) (append (rest x) y))))
;;; The function rev takes Θ(n2) time where n is the length of the list [why?]. Tail recursion can be
;;; used to obtain a linear time algorithm. Implement reverse using tail recursion. See the example
;;; from the lecture on functional programming. Hint: you will need a helper function that takes an
;;; additional argument and is more general than reverse.

(define (rev L)
    (if (null? L)
        '()
        (revH L '())
    )
)

(define (revH L RL)
    (if (null? L)
        RL
        (revH (rest L) (cons (first L) RL))
    ) 
)


;;; Q 3
;;; Implement the function (map f L) which takes two arguments: 1) f a function of a single variable
;;; and 2) L a list of elements in the domain of f. If L = (x1 x2 … xn), the output of the function is the
;;; list ((f x1) (f x2) … (f xn)). E.G. assuming the function sqr, defined by (define (sqr x) (* x x)), the
;;; result of (map sqr ‘(1 2 3 4)) is ‘(1 4 9 16). First try this example using the Racket function map
;;; and then see that you get the same result using your function. Note that Racket’s map function
;;; is more general in that it allows functions of more than one variable when additional lists are
;;; provided.

(define (sqr x) (* x x))

(define (myMap func L)
    (cond
        [(null? L) '()]
        [else (cons (func (first L)) (myMap func (rest L)))]
    )
)

;;; Q 4
;;; Implement the function (reduce f init L) which takes three arguments: 1) f a function of two
;;; variables and 2) L a list of elements in the domain of f, and 3) init the value to be returned when
;;; L = ‘(). Reduce is the same as the Racket function foldr. E.G. (reduce + 0 ‘(1 2 3 4)) is (+ 1
;;; (reduce + 0 ‘(2 3 4)) = (+ 1 (+ 2 (+ 3 (+ 4 0)))). First try the Racket function foldr, i.e. (foldr + 0 ‘(1
;;; 2 3 4)) and then see that your implementation obtains the same result. Try the other examples
;;; from the functional programming in Racket lecture.

(define (reduce func init L)
    (if (null? L)
        init
        (func )
    )
)