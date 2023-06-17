#lang racket


(define-syntax generate-addition-functions
  (syntax-rules ()
    ((_ name-numbers ...)
     (begin
       (define (name a b) (+ a b ...) )
       ...
       ))))
