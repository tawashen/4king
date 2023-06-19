#lang racket


(define (mes-return NAME ENAME c-arg count) ;(((0 0 0 -3 0 #f)
  (if (not (zero? (car c-arg)))
      (cond ((= count 0) (values NAME "技術点が"  (car c-arg)))
            ((= count 1) (values NAME "体力点が" (car c-arg)))
            ((= count 2) (values NAME "幸運点が" (car c-arg)))
            ((= count 3) (values ENAME "技術点" (car c-arg)))
            ((= count 4) (values ENAME "体力点" (car c-arg)))
            (else #f))
      (mes-return NAME ENAME (cdr c-arg) (+ 1 count))))

(let-values (((a b c) (mes-return 'name 'ename (flatten '((0 0 0) (-3 0) #f)) 0)))
(display (format "~aは~aが~aされた"  a b c)))

; (let-values (((a b c) (mes-return 'name 'ename (flatten '((0 0 0) (-3 0) #f)) 0))) (list a b c))

