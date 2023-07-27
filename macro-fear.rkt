#lang racket


(define-syntax (hyphen-define/ok1 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (syntax-case (datum->syntax #'a
                                   (string->symbol (format "~a-~a"
                                                           (syntax->datum #'a)
                                                           (syntax->datum #'b)))) ;syntax?
                    () ;()
         [name ;name=pattern?
          #'(define (name args ...) ;body?
                   body0 body ...)])]))


(define-syntax (hyphen-define/ok2 stx)
    (syntax-case stx () ;stxの部分が
      [(_ a b (args ...) body0 body ...);この形になるとマッチ
       (with-syntax ([name ;name=pattern?
                      (datum->syntax #'a
                                          (string->symbol (format "~a-~a"
                                                                  (syntax->datum #'a)
                                                                  (syntax->datum #'b))))] ;syntax?
                     )
         #'(define (name args ...);body?
             body0 body ...))]))

;(syntax-case <syntax> () [<pattern> <body>])
;(with-syntax ([<pattern> <syntax>]) <body>)


 (require (for-syntax racket/syntax))
 (define-syntax (hyphen-define/ok3 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...);ここまで一緒
       (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)]);datum->syntaxが不要になると
         #'(define (name args ...);template
             body0 body ...))]))

(define-syntax (hyphen-define* stx)
    (syntax-case stx ()
      [(_ (names ...) (args ...) body0 body ...)
       (let ([name-stxs (syntax->list #'(names ...))])
         (with-syntax ([name (datum->syntax (car name-stxs)
                                            (string->symbol
                                             (string-join (for/list ([name-stx name-stxs])
                                                            (symbol->string
                                                             (syntax-e name-stx)))
                                                          "-")))])
           #'(define (name args ...);template
               body0 body ...)))]))


