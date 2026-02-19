#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "mceval-tests.rkt")

(provide mceval-tests)

(define mceval-tests
  (test-suite
   "Metacircular Evaluator Tests"
   basic-tests
   primitive-tests
   let-tests
   and-or-tests
   let*-tests
   force-delay-tests
   streams-tests
   stream-tests))
