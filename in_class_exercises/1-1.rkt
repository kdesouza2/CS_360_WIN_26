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
          (cons (first L) (removeDup (rest L)))))
)

