#lang racket

(require (for-syntax syntax/parse
                     syntax/stx))

(provide (rename-out [bidir-typeck-mb #%module-begin])
                     -> λ if +)

(begin-for-syntax
  (define (infer e)
    (syntax-parse e
      ; values
      [_:boolean #'Bool]
      [_:integer #'Int]
      [_:string  #'String]

      ; if
      [((~literal if) e1 e2 e3)
       #:when (check #'e1 #'Bool)
       #:with τ (infer #'e2)
       #:when (check #'e3 #'τ)
       #'τ]

      ; plus
      [(~literal +) #'(-> Int Int Int)]
      [((~literal +) e1 e2) #'Int]

      ; function application
      [(e1 e2 ...)
       #:with ((~literal ->) τ1 ... τn) (infer #'e1)
       #:when (andmap check
                      (syntax->list #'(e2 ...))
                      (syntax->list #'(τ1 ...)))
       #'τn]

      ; type error
      [e (raise-syntax-error
          'compute
          (format "could not compute type for term: ~a" (syntax->datum #'e)))]))

  (define (check e τ)
    (define τ-ck (infer e))
    (or (type=? τ τ-ck)
        (raise-syntax-error
         'check
         (format "error while checking term ~e: expected ~a; got ~a"
                 (syntax->datum e)
                 (syntax->datum τ)
                 (syntax->datum τ-ck)))))

  (define (type=? τ1 τ2)
    (or (and (identifier? τ1) (identifier? τ2) (free-identifier=? τ1 τ2))
        (and (stx-pair? τ1) (stx-pair? τ2)
             (= (length (syntax->list τ1))
                (length (syntax->list τ2)))
             (andmap type=? (syntax->list τ1) (syntax->list τ2))))))

(define-syntax (bidir-typeck-mb stx)
  (syntax-parse stx
    [(_ e ...)
     #:do [(for-each (lambda (e)
                       (printf "~a | ~a\n"
                               (syntax->datum e)
                               (syntax->datum (infer e))))
                     (syntax->list #'(e ...)))]
     #'(#%module-begin (void))]))
