## CS 360 Practice problems for the Midterm Winter 2026

**[Higher order functions]**

Use map to implement (zip xs ys) where xs and ys are lists of the same length
and the output is a list of pairs.

    (define (zip xs ys)
        (map cons xs ys)
    )

**[Tail Recursion]**

The following function computes the reverse of a list. It is not tail recursive. Implement
a tail recursive reverse function.

    (define (rev l)
        (if (null? l)
            null
            (append (rev (rest l)) (cons (first l) null))))

    (define (revt x y)
        (if (null? x)
            y
            (revt (rest x) (cons (first x) y))
        )
    )

    (define (rev l)
        (revt l '())
    )

**[Substitution Model of Computation]**

Assume that (power a n) is a primitive function that returns a n, (a raised to the n-th power), and
the following function definitions.

    (define (sumf f n)
        (if (= n 0)
            0
            (+ (f n) (sumf f (- n 1)))))

    (define (nth-power n) (lambda (x) (power x n)))

Use the substitution model to evaluate the function application (sumf (nth-power 3) 2). You
should show a sequence of substitutions that lead to the result. What result would you get
using the environment model with dynamic scope?

It would ignore the global definition of x, because since it's a dynamic scope it focuses on the defitions in it's close proximity rather than the global definitions

(sumf (nth-power 3) 2)
(sumf (lambda (x) (power x 3)) 2)
(sumf (x^3) 2)
    (if (= 2 0) 0 (+ (x^3) 2) (sumf (x^3) (- 2 1)))
    (if #f 0 (+ (x^3) 2) (sumf (x^3) (- 2 1)))
    (+ 8 (sumf (x^3) 1))    
        (if (= 1 0) 0 (+ (x^3) 1) (sumf (x^3) (- 1 1)))
        (if #f 0 (+ (x^3) 1) (sumf (x^3) (- 1 1)))
        (+ 1 (sumf (x^3) 0))
            (if (= 0 0) 0 (+ (x^3) 0) (sumf (x^3) (- 0 1)))
            (if #t 0 (+ (x^3) 1) (sumf (x^3) (- 1 1))) 
            0
9 

**[Environment Model of Computation]**

In this question there are a sequence of expressions, and you are to show their effect on various
environments. Review how mceval and mcapply handle applications.
    
    (define (make-adder x) (lambda (y) (+ x y)))
    
    (define add1 (make-adder 1))
    
    (define x 2)
    
    (add1 2)

What’s the environment stored with add1? What’s the environment right after add1 is called? How does this change if dynamic scope is used?

**[Streams]**

• Implement a function to count the number of primes in an interval [a..b] using the stream
paradigm. You may use stream-enumerate-interval, stream-map, stream-filter and stream-
foldr. Could write a function 

(define (charfun pred?) 
    (lambda (x) (if (pred? x) 1 0)))



**[Memorization]**

We saw that normal order evaluation had more arithmetic compared to applicative order in the
computation of (f 5) where
 (define (square x) (* x x))
 (define (sum-of-squares x y) (+ (square x) (square y)))
 (define (f a) (sum-of-squares (+ a 1) (* a 2)))

How much more? How much of a reduction is there in normal order when memoization is used? How is
memoization implemented in this case?

**[mceval]**

What is required to add the unless special form to mceval? Why must it be a special form?

    (define (unless condition usual-value exceptional-value)
        (if condition exceptional-value usual-value))

    (unless (= b 0)
    (/ a b)
    (begin (display "exception: returning 0") 0))
