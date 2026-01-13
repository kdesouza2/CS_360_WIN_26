#lang racket


(define (isMember? x L)
  (cond
    [(null? L) #f]
    [(equal? x (first L)) #t]
    [else (isMember? x (rest L))]
  )  
)

(define (removeDup L)
  (if (null? L)
      '()
      (if (isMember? (first L) (rest L))
          (removeDup (rest L))
          (cons (first L) (removeDup (rest L)))
    )
  )
)

;;; other version of removeDup using cond
(define (remDup L)
  (cond 
    [(null? L) L]
    [(isMember? (first L) (rest L)) (remDup (rest L))]
    [else (cons (first L) (remDup (rest L)))]
  )
)

;;; Notes
;;; (range int) --> List: returns a list of integers from 0 up to the int given exclusively 