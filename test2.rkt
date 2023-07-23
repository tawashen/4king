#lang racket

; Your code here!



;SK (satisfy-item-doreka satify-item palace)
(define (satisfy-item-doreka? card-item player-item)
    (cond ((null? player-item) #f)
          ((member (car player-item) card-item) #t)
          (else (satisfy-item-doreka? card-item (cdr player-item)))))

;王宮関数　テスト待ち
(define palace (lambda (W A) ;world->world '(#f item 30 20 10 #f #f)
                   (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
                        (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
                            (display "君は王の前でパフォーマンスを披露した")
                            (newline)
                            (let ((dice (random 1 7)))
                              (case dice
                                ((1) (display (format "~aを授けよう~%" (list-ref A dice))))
                                ((or 2 3 4) (display "~aゴールドを授けよう~%" (list-ref A dice)))
                                ((5) (display (format "何もやらん！~%")))
                                ((6) (display (format "牢獄行きじゃ！~%"))))                         
                                (WORLD (list-set PLAYERS (car PHASE) (list-set PLAYERS (list-ref PHASE 0)
                                                 (case dice
                                       ((1) (add-item c-player (list-ref A dice)))
                                       ((2) (add-gold c-player (list-ref A dice)))
                                       ((3) (add-gold c-player (list-ref A dice)))
                                       ((4) (add-gold c-player (list-ref A dice)))
                                       ((5) c-player)
                                       ((6) c-player))))
                                       ENEMIES MAPLIST SMAP PMAP PHASE
                                                (case dice
                                                  ((6) (list-set COORD (list-ref PHASE 0) 24))
                                                  (else COORD))
                                                 WIN))))))
                                       
(define (add-item player item) ;player->player
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) player))
               (NAME NAME SKILLP HITP LUCKP EQUIP GOLD (cons item ITEM) SPECIAL STATUS)))

(define (add-gold player gold) ;player->player
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) player))
               (PLAYER NAME SKILLP HITP LUCKP EQUIP (+ gold (PLAYER-GOLD)) ITEMS SPECIAL STATUS)))
           
;S2 `((,silver-short-sword ,war-hammer ,long-sword ,throwing-knife) #f #f)
;S3 `((,magic-glove ,shield ,chain-mail) #f #f)
;S4 `((,horse) #f #f)
;S5 `((,numbing-medicine ,anesthetic ,medicinal-herb ,skill-herb ,power-herb ,luck-herb) #f #f)
;S7 `((,lantern ,shovel ,rope ,lock-pick ,lute) #f #f)
;S8 `((,womans-clothing ,magic-clothing ,black-clothing) #f #f)
;S9 `((,cake) #f #f)
;S10 `((,fake-jowely) #f #f)
(define shop (lambda (W A) ;テスト待ち
    (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
               (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
                    (match-let (((PLAYER  NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
                            (let ((item-list (filter (lambda (x) (> GOLD (ITEM-COST x)) (list-ref A 0)))))
                        (for-each display (cons "[0:買い物を終える]" (map (match-lambda (`(,num ,name . ,cost)
                                                    (format "[~a][~a cost:~a]~%" num name cost)))
                                    (enumerate (map (lambda (x) `(,(ITEM-NAME x) ,(ITEM-COST x))) item-list) 1))))
                                (display "どれを買うかね?") (newline)
                                (let ((answer (string->number (read-line))))
                                     (cond ((= 0 answer) W)
                                           ((> answer (length A)) (shop W A))
                                           (else (begin 
                            (display (format "~aのお買い上げありがとうございます~%") (ITEM-NAME (list-ref item-list (- answer 1))))
                            (let* ((new-item (cons (list-ref item-list (- answer 1)) PLAYER-ITEM))
                                   (new-gold (- GOLD (ITEM-COST (list-ref item-list (- answer 1)))))
                                   (new-player (PLAYER NAME SKILLP HITP LUCKP EQUIP new-gold new-item SPECIAL STATUS))                                   
                                   (new-players (list-set PLAYERS (car PHASE) new-player)))
                              (shop (WORLD new-players ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN A)))))))))))))


;S6 (`(,acrobat ,performance ,handmagic ,instrument) #f #f)
(define skill-shop (lambda (W A) ;スキル取得クロージャ
    (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
               (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
                    (match-let (((PLAYER  NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
                            (let ((item-list (list-ref A 0)))
                              (for-each display (map (match-lambda (`(,num ,name)
                                                                    (format "[~a][~a]~%" num name)))
                                                     (enumerate (map (lambda (x) `(,(ITEM-NAME x))) item-list) 1)))
                              (display "どれを習いたい？") (newline)
                              (let ((answer (string->number (read-line))))
                                     (cond ((= 0 answer) W)
                                           ((> answer (length A)) (skill-shop W A))
                                           (else (begin 
                            (display (format "~aを習得した~%") (ITEM-NAME (list-ref item-list (- answer 1))))
                            (let* ((new-item (cons (list-ref item-list (- answer 1)) PLAYER-ITEM))
                                   (new-hp (cons (- (car HITP) 4) (cdr HITP)))
                                   (new-player (PLAYER NAME SKILLP new-hp LUCKP EQUIP GOLD ITEM SPECIAL STATUS))                                   
                                   (new-players (list-set PLAYERS (car PHASE) new-player)))
                              (shop (WORLD new-players ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN A)))))))))))))
                              
                     


;selectにメッセージを引数として設定する!
;PLAYERのスロットにお尋ねスロットを作ること!
;main-readに「移動しない」を付け加えること！


;ステータスを一気に変更する関数を書こうと思ったが意味ないか・・
(define change-player-status (lambda (W A) ;引数はただのリストで一気に適用する
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
               (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
               (let ((new-player (PLAYER NAME ....)))
               (WORLD ...)))))))


;後はテスト
(define satify-status (lambda (W A)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
               (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))            
               (cond ((satify-status? c-player (list-ref A 0)) W) ;満たしていれば素通り
                     (else ((list-ref A 1) W A)))))))) ;ここではA-LISTにnext-playerを入れておく
                 
;D10 selecet -> poltergeist -> next-player `(() #f #f)
(define poltergeist (lambda (W A)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
                        (let loop ((p-skill (car (PLAYER-SKILLP c-player))) (e-skill (ENEMY-SKILLP (car ENEMIES))) (count 0))
                             (cond ((= 3 count)
                                    (into-world (add-gold c-player 30) W)) ;3回逃げ切ったら30goldゲットして次のクロージャへ
                                   ;into-worldはplayer->world
                                 (else (
                        (display "[0]逃げ出す [1]踏ん張る") (newline)
                        (let ((answer (string->number (read-line))))
                             (cond ((= 0 answer) 
                                   ((= 1 answer) (loop (p-skill e-skill (cond ((> (+ (dice) p-skill) (+ (dice) e-skill)) (+ count 1))
                                                                              (else 0)))))
                                                                          (else (loop p-skill e-skill count))))))))))))

;into-world plyaer->world　後でUtilへ移動
(define (into-world c-player world)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
       (let ((new-players (list-set PLAYERS (car PHASE) c-player)))
         (WORLD new-players ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN))))



;0620 DQ poison-attackを考えるか先頭に付け加える

;DK 戦いを拒否した場合出禁の処理を考える必要あり、お尋ね者と合わせてリストにシンボルをCONS
(define corosseum (lambda (W A)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
                   (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
                      (display "闘技場だ!対戦相手は・・") (newline)
                      (let* ((saikoro (random 1 7))
                                      ((new-enemy (case saikoro)
                                                  ((1) toge)
                                                  ((2) wolf)
                                                  ((3) panther)
                                                  ((4) dragger)
                                                  ((5) cyclops)
                                                  ((6) hole-devil))))
                                              (display (format "~a:HIT[~a] SKILL[~a]だ!~%戦うか?" 
                                                               (ENEMY-NAME new-enemy)
                                                               (ENEMY-HITPP new-enemy)
                                                               (ENEMY-SKILLP new-enemy)))
                            (let ((answer (read-line)))
                                 (cond ((not (string=? answer "y"))
                                        (let* ((new-status (cons 'dekin STATUS))
                                               (new-player (PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL new-status)))
                                        (next-player (into-world new-player W) A)))))))))))
  
                             
;HA ウォーハンマー パワーハンマーのみで攻撃できる 武器種とは別に属性スロットが必要
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
                             

;H2 ('(,pendant 30) #f #f)
;mainとして使うアイテムとゴールドをゲットするクロージャ
(define get-item-and-gold (lambda (W A)) ;A
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
                   (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
                     (into-world
                      (PLAYER NAME SKILLP HITP LUCKP (+ GOLD (cadr (list-ref A 0))) (cons (car (list-ref A 0) ITEM)) SPECIAL STATUS)
                      W)))))
                 
                 
;ITEM-BUY ITEM-SELLを設定するITEM-BUYはITEM-COSTから変更
(define pawn-shop (lambda (W A);質屋クロージャ アイテム構造体に売値を設定する
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
                   (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
               (displayln "何か買い取って欲しいものはあるかい?")
               (let ((can-sell-item (filter (lambda (x) (ITEM-SELL x)) ITEM)))
               (for-each display (cons "[0] やめる" (map (match-lambda (`(,num ,name . cost)
                                                            (format "[~a] ~a:~aゴールド" num name cost))
                                                        (enumerate (map (lambda (x) (cons x can-sell-item ))1))))))
                                                    (let ((answer (string->num (read-line))))
                    (cond ((> answer (length can-equip-list)) (pawn-shop W A));答えがリスト以上だったらやり直し
                          ((<= answer 0) W);0だったらWをスルーで渡す
                          (else
                              (let* ((target-item (list-ref can-equip-list (- answer 1)))
                                     (new-items (delete target-item ITEM))
                                     (new-gold (- gold (ITEM-SELL target-item)))
                                     (new-player (PLAYER NAME SKILLP HITP LUCKP EQUIP new-gold new-items SPECIAL STATUS))
                                     (new-players (list-set PLAYERS (car PHASE) new-player)))
                                 (pawn-shop (WORLD new-players ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) A)))))))))))
                             
;H5手強そう dai->next-player
(define dai (lmabda (W A)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let* ((c-player (list-ref PLAYERS (list-ref PHASE 0)))
                 ;ex (2 3 0 1)の場合(list-ref players 2)が現在のP、[0] [1]店 [2]P3 [3]P0 [4]P1　となり
                 ;(list-ref players (list-ref PHASE (- answer 1))で特定できる
                                (target-players (delete PLAYERS c-player)))
                    (displayln "どこから盗んで欲しいんだ?")
                    (displayln "[0]やめる [1]店" )
                (cond ((< 1 (length PLAYERS)) ;マルチプレイならプレイヤーから盗むのも表示
                    (for-each displayln (map (match-lambda (`(,num ,player-name)
                                                 (format "[~a]~a" num player-name))) target-players
                                             (enumerate (map (lambda (x) (cons ))2)))))
                                         (else (newline)))
                                     (let ((answer (string->num (read-line))));ターゲット番号を入力
                                          (cond ((= answer 0) W);0ならスルーして次のクロージャへ
                                                ((> answer (+ 1 (length PLAYERS))) (dai W A));大きすぎる場合やり直し
                                                ((= 1 answer) (dai-shop W) A) ;world->world
                                                (else (dai-other-player W A answer c-palayer target-players))))))));world->world

;dai-shop ゲーム全体の店からアイテムを盗む(店のアイテムを減らす必要なし)
;大域変数全アイテムのリストall-can-buy-itemsを作っておく
(define (dai-shop W A) ;world->world
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let* ((c-player (list-ref PLAYERS (list-ref PHASE 0)))
                                (target-players (delete PLAYERS c-player)))
                    (for-each displayln (cons "[0]やめる" (map (match-lambda ('(,num . ,name)
                                                            (format "[~a]~a" num name)))
                                                              (enumerate (map (lambda (x y) (cons x (ITEM-NAME y))) all-can-buy-items) 1))))
                                    (displayln "どれを盗んで欲しいんだ?")
                                    (let ((answer (string->num (read-line))))
                                         (cond ((= answer 0) (dai W A));0なら最初に戻る
                                                ((> answer (length all-can-buy-items))) (dai-shop W A));大きすぎる場合やり直し
                                                (else
                                                    (cond ((success-luck? c-player) ;運試し成功した場合
                                                      (let* ((choice-item ((list-ref all-can-buy-items (- answer 1))))
                                                            (new-player (add-item c-player choice-item)))
                                                            (into-world new-player W)))
                                                        (else 
                                                              (let ((new-coord (list-set COORD (car PHASE) 24)))
                                                              (WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE new-coord WIN)))))))))
                                                          
;dai-players
(define (dai-other-players W A answer c-player target-players)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
              (let* ((target-player (list-ref target-players (- answer 2)));0と1を抜かないといけないので2引く
                     (match-let (((PLAYER TNAME TSKILLP THITP TLUCKP TEQUIP TGOLD TITEMS TSPECIAL TSTATUS) target-player))
            (for-each display (cons "[0]:やめる" (map (match-lambda ('(,num ,name)
                                    (format "[~a]~a" num name)))
                                        (enumerate (map (lambda (x y) (cons x (ITEM-NAME y))) TITEMS) 1))))
                                    (displayln "どれを盗んで欲しいんだ?")
                                    (let ((answer (string->num (read-line))))
                                         (cond ((= 0 answer) (dai W A))
                                               ((> answer (length TITEMS)) (dai-players W A answer c-player target-players))
                                               (else 
                                                     (cond ((success-luck? c-player)
                                                 (let* ((choice-item ((list-ref TITEMS (- answer 1))))
                                                            (new-player (add-item c-player choice-item));アイテムを追加したPlayer
                                                            (new-players1 (lise-set PLAYERS (car PHASE) new-player));新たなPlayers1
                                                            (new-target-items (delete TITEMS choice-item));盗まれた後のアイテムリスト
                                   (new-target-player (PLAYER TNAME TSKILLP THITP TLUCKP TEQUIP TGOLD new-target-items TSPECIAL TSTATUS))
                                   (new-players2 (list-set new-players1 (list-ref PHASE (- answer 1)) new-target-player));新たなPlayers2
                                   (WORLD new-players2 ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN))));->world
                                                           (else
                                                             (let ((new-coord (list-set COORD (car PHASE) 24)))
                                                              (WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE new-coord WIN))))))))))))
                                                    
;H6 バッドステータススロット必要、リスト形式で管理。逮捕歴、悪臭



                                     

;H7 satsify-item -> select ->delete-item ->luck-check ->luck-check ->next-player
(define (luck-check W A) ;'(("おおっ成功じゃ" "ああ・・失敗じゃ" ,namari-knife) #f ,add-item)
        (match-let (((PLAYER ...) c-player))
               (cond ((>= (car LUCKP) (dice))
                          (displayln (format "~a" (list-ref (list-ref A 1) 0)))
                          (let ((new-luckp (cons (- (car LUCKP) 1) (cdr LUCKP))))
                               (into-world (PLAYER ... new-luckp ...))))
                    (else (displayln (format "~a ~を手に入れた" (list-ref (list-ref A 1) 1) (ITEM-NAME (list-ref (list-ref A 0) 2))))
                          (let ((new-luckp (cons (- (car LUCKP) 1) (cdr LUCKP)))
                               (next-player (into-world (PLAYER ... ((list-ref A 2) ((list-ref (list-ref A 0) 2))))))))))))
                           

;H8 check-multi ->select ->curse

(define check-multi (lambda (W A) ;multi以外だったら次のターンへ
                        (match-let (((WORLD ...)))
                        (if (> (length PLAYERS) 1)
                            W (next-player W A)))))
                        
                        
;足止めの呪い用にステータスの項目に一度休みを追加、main-readで判定をするように書く!

(define curse (lambda (W A) ;(("だれを狙うね?") (("asidome . 10") ("byouki" . 20) ("si" . 50)))
                  (match-let (((WORLD ...)))
                             (let* ((c-player ...)
                                    (target-players (delete PLAYERS c-player)))
                                  (displayln (format "~a" (list-ref (list-ref A 0) 0)))
                                  (for-each displayln (cons "[0]やめる" (map (match-lambda `(,num ,name) (format "[~a] ~a" num name))
                                                                          (enumerate (map (lambda (x) (PLAYER-NAME x)) target-players 1)))))
                                    (let ((answerT (string->num (read-line))))
                                         (cond ((= answerT 0) (next-player W))
                                               ((> answerT (length PLAYERS)) (curse W A))
                                               (else 
                                                    (for-each displayln (cons "[0]やめる" (map (match-lambda `(,num ,kind ,cost)
                                                                                                (format "[~a] ~a:~aゴールド" num kind cost))
                                                                        (enumerate (map (lmabda (x) (list (car x) (cdr x))) (list-ref A 1)) 1))))
                                                  (let ((target-player (list-ref target-player (- answerT 1))))
                                                       
                                                     なんか打てないのでRakcetで 


;kokokara0723
                                                       
;H9 select ->casino                                        
(define casino (lambda (W A)
                  (match-let (((WORLD ...)))
                             (let ((c-player ...))
                                  (match-let (((PLAYER ...) c-player))
                                             (displayln "いくら賭ける?[1〜5ゴールド]")
                                             (let ((kakekin (string->number (read-line))))
                                                  (cond ((> kakekin GOLD) (displayln "足りないよ") (casino W A))
                                                        ((<= kakekin 0) (next-player W))
                                                        ((> kakekin 5) (displayln "賭けすぎ") (casino W A))
                                                        (else 
                                                              (displayln "どの数字に賭ける?[1〜6]")
                                                              (let ((kakebangou (string->number (read-line))))
                                                  (cond 
                                                        ((<= kakebangou 0) (casino W A))
                                                        ((> kakebangou 6) (displayln "6までだっての") (casino W A))
                                                        (else
                                                            (let ((result-num (random 1 7)))
                                                                 (cond ((member? ITMES 'magic)
                                                                                 (displayln "手品を使ってイカサマをするか?")
                                                                                 (let ((answer (read-line)))
                                                                                      (cond ((string=? answer 'y)
                                                                                                       (cond ((inner-luck? c-player) 
                                                                                                                           (displayln "あんたの勝ち!")
                                                                                                                           (into-world (add-gold (dec-luck c-player) (* 6 kakekin))))
                                                                                                            (else (into-world (to-jok c-player))))))))))))
(define (inner-luck? c-player)
    (match-let (((PLAYER ...) c-player))
               (if (> (car LUCKP) (dice)) #t #f)))
           
(define (dec-luck c-player)
    (match-let (((PLAYER ...) c-player))
               (PLAYER ... (cons (- (car LUCKP) 1) (cdr LUCKP)) ...)))
           

;H10 satisy-items ->luck? ->luck ->next-player
;HQ satisy-item ->battle

;HK
(define cresent (lambda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                                    (match-let (((PLAYER ...) c-player))
                                               (case NAME
                                                     ((dia) (if (win-check? c-player) (you-win c-player)))
                                                     ((heart) (if (win-check? c-player) (you-win c-player)))
                                                     (else (next-player))))))))
                                                 

;CA satisy-item ->panalty-battle ->add-item
(define penalty-battle (lmabda (W A)
                               ;battleを改変してコピー
                               

                               
;C2 select ->luck-ido
(define luck-ido (lmabda (W A)
            (let ((c-player (...)))
                 (match-let (((PLAYER ...) c-player))
            (into-world (change-gold (change-luck c-player 1) (list-ref A 0)) W))
                        
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
           
(define (change-luckp c-player num)
    (match-let (((PLAYER ...) c-player))
               (let ((new-luckp (cond ((> (+ (car LUCKP) 1) (cdr LUCKP)) (cdr LUCKP))
                                      (else (+ (car LUCKP) 1)))))
               (PLYER ... (cons new-luckp (cdr LUCKP))))))
           
(define (change-coord W num)
                    (match-let (((WORLD ...) W))
                               (WORLD ... (list-set COORD (car PHASE) num))))
                                    
    
           
           
;C3 satisfy-gold ->tenbo
(define tenbo (lambda (W A)
                  (let ((c-player ...))
                       (displayln "覗くか?")
                       (let ((answer (read-line)))
                            (cond (((not (string=? asnwer 'y)) next-player W))
                                  (else 
                                        (displayln "どの座標を観たい?[0〜49]")
                                        (let ((answerN (string->num (read-line))))
                                             (cond ((>= answerN 50) tenbo W A)
                                                   (else (displayln (format "~aだな" (CARD-NAME (list-ref *map* asnwerN))))
                                                         (tenbo (into-world (dec-gold c-player 1) W) A))))))))))

;C4 satisfy-gold ->select ->shokudo
;ターン開始時に居残り可能か移動必須かを決める機能追加
(define shokudo (lambda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                                    (next-player (into-world (change-gold (change-hitp c-player 4) -2)) A)))))
                                
;C5 select ->magic-shop
(define magic-shop (lambda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                                    (displayln "どのサービスを受けたいのかな?[0]やめる")
                                    (let ((answer (string->num (read-line))))
                                         (case answer 
                                               ((0) (next-player W))
                                               ((1) (next-player (into-world (change-gold (change-skillp c-player (cdr SKILLP)) -10))))
                                               ((2) (next-player (into-world (change-gold (change-hitp c-player (cdr HITP)) -10))))
                                               ((3) (next-player (into-world (change-gold (change-luckp c-player (cdr LUCKP)) -10))))
                                               ((4) 
                                                    (displayln "どこにジャンプする?[0]やめる")
                                                    (let ((answerN (string->num (read-line))))
                                                         (case answerN
                                                               ((0) (magic-shop W A))
                                                               ((> 49) (magic-shop W A))
                                                               (else (next-player (change-coord (into-world (change-gold c-player -10)) answerN))))))))))))
                                                           
;C6 select ->bath
(define bath (lambda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                                    (next-player (into-world (change-gold (del-status c-player 'stink) -1) W))))))
                                
;C7 gakki ->sake-shop
(define gakki (lmabda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                                    (cond ((inner-satisfy-items? c-player (list-ref A 0))
                                                                 (displayln "楽器演奏するかね?")
                                                                 (let ((answer (read-line)))
                                                                      (cond ((string=? answer 'y')
                                                                                       (cond ((>= (car SKILLP) (dice))
                                                                                                  (into-world (change-gold c-player (random 1 7))))
                                                                                              (else (into-world (change-luckp c-player -1)))))
                                                                            (else W))))
                                                                        (else W))))))
                                                                    
(define sake-shop (lambda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                                    (displayln "お酒を買うかね?")
                                    (let ((answer (read-line)))
                                         (cond ((not (string=? answer 'y)) W)
                                               (else (into-world (change-gold (add-item c-player 'sake) -2)))))))))
                                           
;C8 select ->togi
;ITEMの武器の種類にNormalとかSilverとかMagicとかをつくる
;戦闘開始時に使う武器を選ぶようにする
(define togi-shop (lambda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                                    (let ((blade-list (filter (lambda (x) (string=? 'normal-blade (ITEM-KIND x))) ITEMS)))
                                         (for-each displayln (cons "[0]やめる" (map (match-lambda ((,num ,name)
                                                                                       (format "[~a]~a" num name))) 
                                                                                   (enumerate (map (lambda (y) (ITEM-NAME y)) blade-list) 1))))
                                                                               (let ((answerN (read-line)))
                                                                                    (cond ((= 0 answerN) (next-player W))
                                                                                          ((> (length blade-list) answerN) (togi-shop W A))
                                                                                          (else
                                                                                              (let ((new-blade (list-ref blade-list (- (answerN) 1))))
                                                                                                   (match-let (((ITEM ...) new-blade))
                                                                                                              (match-let (((PLAYER ...) c-player)))
                                                                                                              (into-world (change-gold 
                                                                                                                          (PLAYER ...(cons (ITEM ... (+ 1 POWER) ...) (del-list ITEMS new-blade)))
                                                                                                                          -10)
                                                                                                                          W)))))))))))
                                                                                                                      
;C9 select ->shop ->guild
(define guild-shop (lambda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                  (displayln "何か技能を学ぶかね?")
                  (for-each displayln (cons "[0]やめる" (map (match-lambda ((,num ,name)
                                                                               (format "[~a] ~a" num name)))
                                                                           (enumerate (map (lambda (y) (ITEM-NAME y)) (list-ref A 0)) 1))))
                                                                       (let ((answerN) (string->num (read-line)))
                                                                            (cond ((= 0 answerN) W)
                                                                                  ((> answerN (length (list-ref A 0)) guild-shop W A))
                                                                                  (else
                                                                                      (match-let (((PLAYER ...) c-player))
                                                                                                  (into-world (change-hitp 
                                                                                                              (PLAYER ...(cons (list-ref A (- answerN 1)) ITEMS) ...)
                                                                                                              (cond ((doreka-itemu (list-ref A 0) ITEMS) -4)
                                                                                                                    (else -6)))
                                                                                                              (next-player W))))))))))


;リストからアイテムを除去
(define (delete-item-list item lst new-lst)
    (cond ((null? lst) new-lst)
          ((equal? item (car lst)) (delete-item-list item (cdr lst) new-lst))
          (else (delete-item-list item (cdr lst) (cons (car lst) new-lst)))))
                                                                                                          
;C10 傭兵はアイテム扱いにして戦闘システムに組み込むことにする
(define youhei (lambda (W A)
                    (match-let (((WORLD ...) W))
                               (let ((c-player (...)))
                                    (next-player (into-world (change-gold (add-item c-player 'youhei) -30 W)))))))
                                
;CQ satisy-item ->battle

;CK
(define bunta (lambda (W A)
                  ))
                                    
                                                           

