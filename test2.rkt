#lang racket

; Your code here!



;SK (satisfy-item-doreka satify-item palace)
(define (satisfy-item-doreka? card-item player-item)
    (cond ((null? player-item) #f)
          ((member (car player-item) card-item) #t)
          (else (satisfy-item-doreka? card-item (cdr player-item)))))
      
(define palace (lambda (W A)
                   (match-let (((WORLD ...)))
                        (let ((c-player (...)))
                            (display "君は王の前でパフォーマンスを披露した")
                            (newline)
                            (let ((dice ((random 1 7))))
                                 (case dice
                                       ((1) (add-item c-player ))
                                       ((2) (add-gold c-player))
                                       ((3) (add-gold c-player))
                                       ((4) (add-gold c-player))
                                       ((5) (display "何ももらえない"))
                                       ((6) (force-coord 24))))))))
(define (add-item player item)
    (match-let (((PLAYER ...)))
               (let ((new-player (NAME ... (cons item ITEM))))
                    (WORLD ... new-player))))
(define (add-gold player gold)
    (match-let (((PLAYER ...)))
               (PLAYER ... (+ gold (PLAYER-GOLD)))
               ))
           
;DA
(define shop (lambda (W A))
    (match-let (((WORLD ..)))
               (let ((c-player ...))
                    (match-let (((PLAYER ...)))
                            (let ((item-list (filter (lambda (x) (> GOLD (ITEM-COST x)) (list-ref A 0))))
                        (for-each display (map (match-lambda (`(,num ,name . ,cost)
                                                    (format "[~a][~a cost:~a]~%" num name cost)))
                                    (enumerate (map (lambda (x) `(,(ITEM-NAME x) ,(ITEM-COST x))) A) 1)))
                                (display "どれを買うかね?") (newline)
                                (let ((answer (string->number (read-line))))
                                     (cond ((= 0 answer) (next-player W A))
                                           ((> answer (length A)) (shop W A))
                                           (else (begin 
                            (display (format "~aのお買い上げありがとうございます~%") (ITEM-NAME (list-ref item-list (- answer 1))))
                            (shop (WORLD ... (cons (list-ref item-list (- answer 1)) PLAYER-ITEM) (- GOLD (ITEM-COST (list-ref item-list (- answer 1))))) A)))))))))))
                                           
;selectにメッセージを引数として設定する!
(define change-player-status (lambda (W A) ;引数はただのリストで一気に適用する
     (match-let* (((WORLD ..))
                   ((PLAYER ...))
                   ((CARD ...)))
               (new-player (PLAYER NAME ....))
               (WORLD ...))))

;PLAYERのスロットにお尋ねスロットを作ること!

(define satify-status (lambda (W A)
     (match-let* (((WORLD ..))
                   ((PLAYER ...))                         
                   ((CARD ...)))
               (let ((c-plyaer ...))
               (cond ((satify-status? c-player card-status) (WORLD ...)) ;満たしていれば素通り
                     (else ((list-ref A 1) W A))))))) ;ここではA-LISTにnext-playerを入れておく
                 
                 
(define poltergeist (lambda (W A)
   (match-let* (((WORLD ..))
                   ((PLAYER ...))                         
                   ((CARD ...)))
                        (let loop ((p-skill (car (PLAYER-SKILLP))) (e-skill (ENEMY-SKILLP)) (count 0))
                             (cond ((= 3 count) ((list-ref A 1))
                                 (else (
                        (display "[0]逃げ出す [1]踏ん張る") (newline)
                        (let ((answer (string->number (read-line))))
                             (cond ((= 0 answer) ((list-ref A 0) W))
                                   ((= 1 answer) (loop (p-skill e-skill (cond ((> (+ (dice) p-skill) (+ (dice) e-skill)) (+ count 1))
                                                                              (else 0)))))
                                                                          (else (loop p-skill e-skill count))))))))))))






