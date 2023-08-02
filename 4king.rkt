#lang racket












;ここにしか置けない関数

















                                                 
;;;;;;;;;;;;;;;;;アイテムテーブル
(hash-set! jack-table 'item-sord sword)

;;;;;;;;;;;;;;;;;アイテム属性テーブル 装備コマンドの時参照する感じ？



;;;;;;;;;;;;;;;;;イベントテーブル
(hash-set! jack-table 'direct go-direct)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;バトル部分
;(define (battle-print w))
;(define (battle-read w))
;(define (battle-eval w))
;(define (battl-loop w))
;(define battle (lambda (x y) ()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;メイン部分
;メインPRINT マップ表示                             
;(define (main-map-print w)

  ;(display (hash-ref jack-table (CARD-MES (PLAYER-COORD (WORLD-PLAYERS w))))))


;(define (main-eval w) ;テスト用仮main-eval
 ; (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) w)
  ;            (let ((c-card SA))
   ;             (foldl 


  
;メインLOOP　次のプレイヤーか自身の次ターン
;(define (main-loop w))



;select 戦うかどうか選択のクロージャ 引数はworld
#;(define select-battle (lambda (x)
                 (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) x))
                   (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                         (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
                     (display "受けるか?") (newline)
                     (let ((answer (read-line))) 
                       (if (string=? "y" answer) ;戦闘を受ける場合
                           (if (and (CARD-FIRST c-card) ;CARD-FIRSTの真偽
                                    (satisfy-item? (cadr (CARD-FIRST c-card)) (PLAYER-ITEMS c-player)));必要アイテムを持っている？
                               ((hash-ref jack-table (car (CARD-FIRST c-card))) x) ;真ならCARD-FIRSTのキーで発動
                               ((hash-ref jack-table 'BATTLE) x)) ;#fならすぐにバトル開始
                           (main-read (WORLD PLAYERS MAPLIST SMAP PMAP (circular PHASE) COORD)) ;受けない場合次へ
                           ))))))



;(hash-set! jack-table 'SELECT select-battle)

           
;luck 運試し
#;
(define luck (lambda (x)
                (match-let (((WORLD PLAYERS MAPLIST SMAP PMAP PHASE COORD WIN) x))
                 (let ((c-card  (list-ref test-zihuda-list (list-ref COORD (list-ref PHASE 0)))))
                  (match-let (((CARD NAME KIND FIRST SECOND MES ENEMY ITEM GOLD ON FLIP) c-card));現在のカード                         
                   (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))));今のPLAYERインスタンス
                     (if (>= (car (PLAYER-LUCKP c-player)) (dice));運試し実行
                         ((hash-ref jack-table 'BATTLE) (WORLD PLAYERS MAPLIST SMAP PMAP PHASE COORD #t))
                         ;↑#tならCARDの成功効果を適用してBATTLEへ。実際はBATTLEで処理、最後の#tがBATTLEでの判定用
                         (main-read (WORLD (list-set PLAYERS
                                           (change-status PLAYERS PHASE (list-ref (CARD-FIRST c-card) 3)))
                                           MAPLIST SMAP PMAP (circular PHASE)
                                           (force-coord COORD PHASE (list-ref (list-ref (CARD-FIRST) 3) 3) #f)))
                         ;↑#fならCARDの失敗処理を施したデータで構造体を作り直して次のプレイヤーへ
                         )))))))

(hash-set! jack-table 'LUCK luck)



;BATTLE関数 一時コメントアウト





;(battle-read world)
  

                                

                  
;;;;;;;;;;;ここまで


                     
                     
               

;(main-print world)
;(main-read world)









