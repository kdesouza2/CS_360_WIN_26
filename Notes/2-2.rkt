#lang racket
(require math/number-theory)

;;; In Racket, binding links an identifier (like a variable name) to a value, function, or syntactic form, 
;;; defining its scope and meaning within a specific part of the program using forms like define, let, lambda, 
;;; or require

;;; Streams 
    ;;; A stream is a (possibly infinite) sequence of values that are computed on-demand. A stream can be though of as a
    ;;; “delayed” or “lazy” list, that is, a list whose elements are not computed until they are needed. Streams are useful
    ;;; for representing very large—possibly infinite!—sequences of values in a memory-efficient manner. In this sense,
    ;;; they are very similar to Python generators. Like generators, streams allow us to write compositional code that
    ;;; manipulates sequences of values without paying a huge memory penalty.

    ;;; Sequence of Streams
        ;;; 1. (cons-stream x y)
        ;;; 2. (stream-car s)
        ;;; 3. (stream-cdr s)
        ;;; 4. (stream-null? s)
        ;;; 5. the-empty-stream

;;; Consider the following example:

(define nums
  (stream-cons 1
    (stream-cons 2
      (stream-cons 3 empty-stream)))
)

(define (naturals n)
  (stream-cons n (naturals (+ n 1))))

;;; Consider this following example:

(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ low 1) high))
    )
)

(car (cdr (filter prime?
(enumerate-interval 10000 1000000))))