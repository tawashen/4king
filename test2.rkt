#lang racket


(require "4king-data.rkt")

(define test `(,SA ,S2 ,S3 ,S4 ,S5 ,S6 ,S7))
(map (lambda (x) (CARD-NAME x)) test) 
