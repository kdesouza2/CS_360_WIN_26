## Midterm Review

1. What invariant does the substitution model of evaluation require? How does set! break that invariant?

2. What are the steps for evaluating a compound expression that is not a special form?

3. What are the steps for applying a compound procedure in the substitution model of evaluation?

4. What are the steps for applying a compound procedure in the environment model of evaluation?

5. Give a desugaring of the following let* expression into an expression that does not use let* . Your desugaring must have the same meaning as the original let* expression.

(let ((x 1))
  (let* ((x 2)
        (y x))
    (+ x y)))

6. The language construct unless can not be implemented as a function in (applicative) Scheme. Show how to implement it as a derived expression.

## Notes
Assignment in Racket --> set! <br>
How is set! implemented: <br>
  Look in env <br>
  Nearest one in scope is changed to new value <br>

Thunk is a function of no arguments that store expressions we want to delay