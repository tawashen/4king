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

;CARDの指定アイテムのどれかを持っているかどうか
(define (satisfy-item-doreka? card-item player-item)
    (cond ((null? player-item) #f)
          ((member (car player-item) card-item) #t)
          (else (satisfy-item-doreka? card-item (cdr player-item)))))

;PLAYERにアイテムを追加する
(define (add-item player item) ;player->player
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) player))
               (NAME NAME SKILLP HITP LUCKP EQUIP GOLD (cons item ITEM) SPECIAL STATUS)))


;PLAYERの任意のステタースが条件を満たしているかチェック
(define satisfy-status? (lambda (W A)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
               (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))            
               (cond ((satisfy-status? c-player (list-ref A 0)) W) ;満たしていれば素通り
                     (else ((list-ref A 1) W A)))))))) ;ここではA-LISTにnext-playerを入れておく

;into-world plyaer->world 後でUtilへ移動
;PLAYERをWORLDに入れる
(define (into-world c-player world)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
       (let ((new-players (list-set PLAYERS (car PHASE) c-player)))
         (WORLD new-players ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN))))


;装備変更関数
(define (equip-change? c-player) ;player->player
                          (displayln "変更する装備を選べ")
                          (displayln "[0]やめる [1]武器 [2]鎧 [3]盾 [4]服 [5]手袋")
                          (let ((answer (read-line)))
                               (case answer
                                     ((>= answer 5 (equip-change? c-player))
                                     ((0) c-player);やめた場合そのままPlayerを返す
                                     ((1) (equip-change c-player 'weapon))
                                     ((2) (equip-change c-player 'armor))
                                     ((3) (equip-change c-player 'shield))
                                     ((4) (equip-change c-player 'cloth))
                                     ((5) (equip-change c-player 'glove))))))

(define (equip-change c-player kind) ;player->player
   (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
    (let ((can-equip-list (filter (lambda (x) (stirng=? kind (ITEM-KIND x))) ITEMS)));Kind引数にマッチするものをアイテムからフィルター
         (cond ((null? can-equip-list) (displayln "ねえよ!") (equip-change? c-player));対応物が無かったら装備するか?へ戻す
               (else 
    (let ((plus-num-list (map (lambda (num item) (cons num item)) (iota (length can-equip-list) 1 1) can-equip-list)));番号をリストにくっつける
    (for-each display (map (lambda (x) (format "[~a] ~a~%") (car x) (cdr x)) plus-num-list)) ;Noと装備候補表示
    (let ((answer (string->num (read-line))))
         (cond ((> answer (length can-equip-list)) equip-change c-player kind);答えがリスト以上だったらやり直し
               ((<= answer 0) (equip-change? c-player));0だったら装備するか?に戻す
               (else
                   (let* ((new-player (to-item-list c-player kind));現在装備しているものをItemに戻す
                          (new-equip (list-ref can-equip-list (- answer 1)));選んだアイテムをEquipにコピー
                          (new-items (delete new-equip ITEM));Equipに指定したアイテムをItemから削除
                        (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) new-player))
                                   (PLAYER NAME SKILLP HITP LUCKP (cons new-equip EQUIP) GOLD new-items SPECIAL STATUS)))))))))))))

;現在装備している装備をItemに戻す関数
(define (to-item-list player kind) ;player->player
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) player))
               (let* ((new-equip (filter (lambda (x) (not (string=? kind x))) EQUIP))
                                 (new-item (filter (lambda (x) (string=? kind x) EQUIP))))
                             (PLAYER NAME SKILLP HITP LUCKP new-equip GOLD (cons new-item ITEM) SPECIAL STATUS))))


(define (inner-luck? c-player)
    (match-let (((PLAYER ...) c-player))
               (if (> (car LUCKP) (dice)) #t #f)))
          
(define (change-luck c-player num)
    (match-let (((PLAYER ...) c-player))
               (let ((new-luckp (cond ((> (+ (car LUCKP) 1) (cdr LUCKP)) (cdr LUCKP))
                                      (else (+ (car LUCKP) 1)))))
               (PLYER ... (cons new-luckp (cdr LUCKP))))))
           
(define (change-gold c-player num)
    (match-let (((PLAYER ...) c-player))
               (PLAYER ... (+ gold num) ...)))

(define (change-hitp c-player num)
    (match-let (((PLAYER ...) c-player))
               (let ((new-hitp (cond ((> (+ (car HITP) 1) (cdr HITP)) (cdr HITP))
                                      (else (+ (car HITP) 1)))))
               (PLYER ... (cons new-hitp (cdr HITP))))))
           
(define (change-skillp c-player num)
    (match-let (((PLAYER ...) c-player))
               (let ((new-skillp (cond ((> (+ (car SKILLP) 1) (cdr SKILLP)) (cdr SKILLP))
                                      (else (+ (car SKILLP) 1)))))
               (PLYER ... (cons new-skillp (cdr SKILLP))))))
           
           
(define (change-coord W num)
                    (match-let (((WORLD ...) W))
                               (WORLD ... (list-set COORD (car PHASE) num))))

;リストからアイテムを除去
(define (delete-item-list item lst new-lst)
    (cond ((null? lst) new-lst)
          ((equal? item (car lst)) (delete-item-list item (cdr lst) new-lst))
          (else (delete-item-list item (cdr lst) (cons (car lst) new-lst)))))
                                    



;未使用関数など

#|
;ステータスを一気に変更する関数を書こうと思ったが意味ないか・・
(define change-player-status (lambda (W A) ;引数はただのリストで一気に適用する
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
               (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
               (let ((new-player (PLAYER NAME ....)))
               (WORLD ...)))))))
|#





