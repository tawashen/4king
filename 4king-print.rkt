#lang racket

(require 2htdp/universe 2htdp/image lang/posn)
(require srfi/1)
(require srfi/13)
(require racket/struct)
(require racket/match)
(provide (all-defined-out))
;(require "4king-data.rkt")





;(define *card-list* (map string->symbol (for*/list ((s suit) (n num)) (string-append s n))))
#;
(define *zihuda-list* `(,SA ,S2 ,S3 ,S4 ,S5 ,S6 ,S7 ,S8 ,S9 ,S10
                            ,DA ,D2 ,D3 ,D4 ,D5 ,D6 ,D7 ,D8 ,D9 ,D10
                            ,HA ,H2 ,H3 ,H4 ,H5 ,H6 ,H7 ,H8 ,H9 ,H10
                            ,CA ,C2 ,C3 ,C4 ,C5 ,C6 ,C7 ,C8 ,C9 ,C10))
#;(define *q&k-list* `(,SQ ,SK ,DQ ,DK ,HQ ,HK ,CQ ,CK))



;(define num-list '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
;(define q-to-k-list '(#\Q #\K))
;(define zihuda (filter (lambda (x) (member (string-ref (symbol->string x) 1) num-list)) *card-list*))
;(define q-to-k (filter (lambda (x) (member (string-ref (symbol->string x) 1) q-to-k-list)) *card-list*))














