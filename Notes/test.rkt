#lang racket
(require racket/mpair)

(define (get-vars exp)
  (map car (cadr exp))
)

(define (let-values exp)
  (map cadr (cadr exp))
)

(define (let-body exp)
    (car (cddr exp))
)

