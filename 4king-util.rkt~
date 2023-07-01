#lang racket

(require "4king-data.rkt")
(require "4king-print.rkt")
(provide (all-defined-out))



;cametanさん製Enumrate関数
(define enumerate
 (case-lambda
  ((seq) (enumerate seq 0))
  ((seq start) (map (lambda (x y)
           (cons x y))
          (range start (+ (length seq) start))
          seq))))


;プロンプト付きINPUT関数
(define input
 (case-lambda
  (() (input ""))
  ((prompt) (display prompt)(newline)
       (read-line))))


;;;;;;;;;;;;;;;;;;移動用チェック述語
(define (hidari-ue? current)
  (if (= 0 current) #f #t))
(define (migi-ue? current)
  (if (= 6 current) #f #t))
(define (hidari-sita? current)
  (if (= 42 current) #f #t))
(define (migi-sita? current)
  (if (= 48 current) #f #t))
(define (hidari? current)
  (if (= 0 (modulo current 7)) #f #t))
(define (migi? current)
  (if (= 0 (modulo (+ 1 current) 7)) #f #t))
(define (ue? current)
  (if (> 7 current) #f #t))
(define (sita? current)
  (if (< 42 current) #f #t))



;PHASE用Circular関数
(define (circular lst)
  (flatten (cons (cdr lst) (car lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;関数
(define (change-coord direct w num)
  (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) w))
    (let* ((new-coord (list-set COORD PHASE (+ (list-ref COORD PHASE) num)))
           (new-pmap (make-players-map new-coord)))
      (WORLD PLAYERS MAPLIST SMAP new-pmap PHASE new-coord WIN))))
  
  
(define (remove-empty-lists lst)
  (cond
    [(empty? lst) empty] ; リストが空なら空を返す
    [(empty? (first lst)) (remove-empty-lists (rest lst))] ; リストの先頭が空なら削除して再帰
    [(list? (first lst)) (cons (remove-empty-lists (first lst)) ; リストの先頭がリストなら再帰的に削除して結合
                               (remove-empty-lists (rest lst)))]
    [else (cons (first lst) (remove-empty-lists (rest lst)))] ; それ以外はそのまま結合
  )
)

;整形後のPlayers配置マップを作る関数、これで移動後にworldを作り直す
(define (make-players-map new-coord)
  (split-list (map align-string (map (lambda (x) (number->string x)) (put-player *player-zero* 1 new-coord)))))

;判定用サイコロ関数
(define (dice)
  (+ (+ 1 (random 6)) (+ 1 (random 6))))
   
;LIST内の特定の要素の場所を返す
(define (list-index2 num lst count)
  (cond ((null? lst) #f)
         ((= num (car lst)) count)
         (else (list-index2 num (cdr lst) (+ 1 count)))))


;敵ごとにランダムで味方へ攻撃対象を決める(IndexのListで)
(define (random-list e-num p-num e-attack-list)
	(if (= 0 e-num) (reverse e-attack-list)
		(random-list (- e-num 1) p-num (cons (random 1 (+ 1 p-num)) e-attack-list))))

;多次元リストを1次元リストに
(define (flat-list lst new-list)
  (if (null? lst) (reverse new-list)
      (flat-list (cdr lst) (append
      (let loop ((lst2 (car lst)) (new-list0 '()))
        (if (null? lst2) new-list0
            (loop (cdr lst2) (cons (car lst2) new-list0))))
            new-list))))

;(random-list (length '(1 2 3 4)) (length '(1 2 3 4)) '())


;CARDが要請するアイテムをPLAYERが全て持っているか?をチェックする関数
(define (satisfy-item? card-item player-item)
  (if (null? card-item)
      #t
      (if (member (car card-item) player-item)
          (satisfy-item? (cdr card-item) player-item)
          #f)))




