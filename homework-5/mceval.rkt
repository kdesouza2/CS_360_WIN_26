#lang racket/base
(require (except-in racket force delay))
(require racket/mpair)

;;;;Ported to Racket by Geoffrey Mainland <mainland@drexel.edu>

;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Evaluate a top-level expression using top-mceval

;;;SECTION 4.1.1

(define (mceval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mceval (cond->if exp) env))
        ((and? exp) (eval-and (and-expr exp) env))
        ((or? exp) (eval-or (or-expr exp) env))
        ((let*? exp) (mceval (let*->nested-lets exp) env))
        ((delay? exp) (mceval (delay->lambda exp) env))
        ((force? exp) (mcapply (mceval (force-exp exp) env) '()))
        ((stream? exp) (mceval (stream->stream-cons exp) env))
        ((let? exp) (mceval (let->application exp) env))
        ((stream-cons? exp)
          (cons (mceval (list 'delay (cadr exp)) env)
                (mceval (list 'delay (caddr exp)) env)))

        ((stream-empty? exp)   ;; empty-stream
         '())

        ((stream-empty?? exp)  ;; stream-empty?
         (null? (mceval (cadr exp) env)))

        ((stream-first? exp)
         (car (mceval (cadr exp) env)))

        ((stream-rest? exp) (mcapply (cdr (mceval (cadr exp) env)) '()))
        ((application? exp)
         (mcapply (mceval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (mcapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mceval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mceval (if-predicate exp) env))
      (mceval (if-consequent exp) env)
      (mceval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mceval (first-exp exps) env))
        (else (mceval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mceval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mceval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp)  true)
        ((string? exp)  true)
        ((char? exp)    true)
        ((boolean? exp) true)
        (else           false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (stream? exp)
  (tagged-list? exp 'stream))

(define (stream->stream-cons exp)

  (define (expand exprs)
    (if (null? exprs)
        'empty-stream
        (list 'stream-cons
              (car exprs)
              (expand (cdr exprs)))))

  (expand (cdr exp)))



(define (stream-empty?? exp)
  (tagged-list? exp 'stream-empty?))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let-variables exp)
  (map car (let-bindings exp)))

(define (let-values exp)
  (map cadr (let-bindings exp)))

(define (let->application exp)
  (cons (make-lambda (let-variables exp)
                     (let-body exp))
        (let-values exp)))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*-bindings exp)
  (cadr exp))

(define (let*-body exp)
  (cddr exp))

(define (let*->nested-lets exp)
  (define (expand bindings body)
    (if (null? bindings)
        (sequence->exp body)
        (list 'let
              (list (car bindings))
              (expand (cdr bindings) body))))
  (expand (let*-bindings exp)
          (let*-body exp)))

(define (delay? exp)
  (tagged-list? exp 'delay))

(define (force? exp)
  (tagged-list? exp 'force))



(define (delay-exp exp) (cadr exp))
(define (force-exp exp) (cadr exp))

(define (delay->lambda exp)
  (list 'lambda '() (delay-exp exp)))

(define (stream-cons? exp)
  (tagged-list? exp 'stream-cons))

(define (stream-empty? exp)
  (tagged-list? exp 'empty-stream))

(define (stream-empty?-exp exp)
  (cadr exp))

(define (stream-first? exp)
  (tagged-list? exp 'stream-first))

(define (stream-rest? exp)
  (tagged-list? exp 'stream-rest))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp)  (tagged-list? exp 'or))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (and-expr exp) (cdr exp))

(define (or-expr exp)  (cdr exp))

(define (eval-and exps env)

  (cond
    ((no-operands? exps) true)

    ((last-exp? exps)
     (mceval (first-exp exps) env))

    (else
     (let ((value (mceval (first-exp exps) env)))

       (if (false? value)
           false
           (eval-and (rest-exps exps) env))))))

(define (eval-or exps env)

  (cond
    ((no-operands? exps) false)

    ((last-exp? exps)
     (mceval (first-exp exps) env))

    (else
     (let ((value (mceval (first-exp exps) env)))

       (if (false? value)
           (eval-or (rest-exps exps) env)
           value)))))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables (list->mlist values)))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'empty-stream '() initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        (list '+ +)
      (list '- -)
      (list '* *)
      (list '/ /)
      (list '< <)
      (list '<= <=)
      (list '= =)
      (list '>= >=)
      (list '> >)

      (list 'error
          (lambda ()
            (error "Metacircular Interpreter Aborted")))
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (top-mceval exp)
  (let ((val (mceval exp (setup-environment))))
    (user-print val)))

;; DO NOT REMOVE
;;
;; Do not try to use Racket's eval. Use mceval!
(define (eval . args)
  (error "Do not call eval"))

(define the-global-environment (setup-environment))

(define input-prompt "> ")

(define (driver-loop)
  (display input-prompt)
  (when (with-handlers
            ([exn:fail? (lambda (exn)
                          (display "Error: ")
                          (display (exn-message exn))
                          (newline)
                          #t)])
          (let ([input (read)])
            (if (eof-object? input)
                (begin
                  (newline)
                  #f)
                (let ([output (mceval input the-global-environment)])
                  (user-print output)
                  (newline)
                  #t))))
    (driver-loop)))

(define (main . argv)
  (driver-loop))

(provide mceval
         setup-environment
         main)