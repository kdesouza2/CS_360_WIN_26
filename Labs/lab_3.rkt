#lang racket
(require racket/stream)
(require racket/trace)
(require math/number-theory)


(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ low 1) high))
    )
)

;;; (stream-first  
;;;     (stream-rest
;;;         (stream-filter prime?
;;;             (stream-enumerate-interval 10000 1000000))))

