#lang racket

;(require srfi/1)
(require racket/struct)
(require racket/match)
(require "4king-data.rkt")
(require "4king-util.rkt")


(define SA (CARD "♠A" 'SELECT ;初期Eval用キー ここではselectクロージャを呼び出す ;CARD-KIND
                 '(LUCK-TRY (numbing-medicine wine) (enemy SKILLP -3) (player HITP -2 24));CARD-FIRST
                 ;↑#tで続く()で必要アイテム、次の()で成功効果 最後の()で失敗効果最後の真偽はJOK(24)行きかどうか
                 '(LUCKP -2) ;存在した場合は降服可能、BATTLEで参照してメニューを出す CARD-SECIND
                 'mes-s1 (list zakura) (list rune-blade) #f #t #t))


;SELECT?> SATISFY-ITEM? > LUCK-TRY? > BATTLE
;ALISTの中にNoの場合の行き先を含める

;select 戦うかどうか選択のクロージャ 引数はworld
(define select? (lambda (W A)
                 (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
               ;    (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                ;         (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
                     (display "受けるか?") (newline)
                     (let ((answer (read-line))) 
                       (if (string=? "y" answer) ;戦闘を受ける場合
                           W
             ;              (if (and (CARD-FIRST c-card) ;CARD-FIRSTの真偽
              ;                      (satisfy-item? (cadr (CARD-FIRST c-card)) (PLAYER-ITEMS c-player)));必要アイテムを持っている?
               ;                ((hash-ref jack-table (car (CARD-FIRST c-card))) x) ;真ならCARD-FIRSTのキーで発動
                ;               ((hash-ref jack-table 'BATTLE) x)) ;#fならすぐにバトル開始
                 ;          (main-read (WORLD PLAYERS MAPLIST SMAP PMAP (circular PHASE) COORD)) ;受けない場合次へ
                  ;         ))))))


                           
