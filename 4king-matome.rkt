#lang racket

(require srfi/1)
(require srfi/13)
(require racket/struct)
(require racket/match)

;初期値;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;4人のジャックテーブル
(define jack-table (make-hash))



;構造体　;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct CARD (NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP) #:transparent)
(struct ENEMY (NAME SKILLP HITP) #:transparent)
(struct ITEM (NAME COST KIND ATT POWER) #:transparent)
(struct PLAYER (NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) #:transparent)
(struct WORLD (PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) #:transparent)

;データ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;メッセージ関連;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;メッセージテーブル
(hash-set! jack-table 'mes-read "~aのターン~% どこへ移動する?北[r] 西[d] 南[c] 東[f]")
(hash-set! jack-table 'mes-s1 "SAだよ")
(hash-set! jack-table 'mes-s2 "S2だよ")
(hash-set! jack-table 'mes-s3 "S3だよ")
(hash-set! jack-table 'mes-s4 "S4だよ")
(hash-set! jack-table 'mes-s5 "S5だよ")
(hash-set! jack-table 'mes-s6 "S6だよ")
(hash-set! jack-table 'mes-s7 "S7だよ")
(hash-set! jack-table 'mes-s8 "S8だよ")
(hash-set! jack-table 'mes-s9 "S9だよ")
(hash-set! jack-table 'mes-s10 "S10だよ")
(hash-set! jack-table 'mes-sq "SQだよ")
(hash-set! jack-table 'mes-sk "SKだよ")
(hash-set! jack-table 'mes-d1 "DAだよ")
(hash-set! jack-table 'mes-d2 "D2だよ")
(hash-set! jack-table 'mes-d3 "D3だよ")
(hash-set! jack-table 'mes-d4 "D4だよ")
(hash-set! jack-table 'mes-d5 "D5だよ")
(hash-set! jack-table 'mes-d6 "D6だよ")
(hash-set! jack-table 'mes-d7 "D7だよ")
(hash-set! jack-table 'mes-d8 "D8だよ")
(hash-set! jack-table 'mes-d9 "D9だよ")
(hash-set! jack-table 'mes-d10 "D10だよ")
(hash-set! jack-table 'mes-dq "DQだよ")
(hash-set! jack-table 'mes-dk "DKだよ")

;CARD関連;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ITEMインスタンス アイテムはリストにしなくても良い?
(define sword (ITEM "剣" #f 'weapon 'one-hand '(0 0))) 
(define wine (ITEM "ワイン" #f 'item 'drink '(0 0)))
(define rune-blade (ITEM "ルーンブレード" #f 'weapon 'one-hand '(2 0)))                 
(define zakura (ENEMY "戦士ザクラ" 12 12))

                

(define silver-short-sord (ITEM "銀の短剣" 10 'weapon 'sacred '(0 0)))
(define war-hammer (ITEM "ウォーハンマー" 35 'weapon 'physical-slayer '(0 0)))
(define long-sword (ITEM "ロングソード" 30 'weapon 'two-handed '(1 0)))
(define throwing-knife (ITEM "投げナイフ" 10 'item 'preemptive-strike '(0 2)))
;(define S2 (CARD "♠2" 'S 2 'SHOP 'mes-s2 '() `(,silver-short-sord ,war-hammer ,long-sword ,throwing-knife) #f #t #f))

(define magic-gloves (ITEM "魔法の手袋" 15 'glove 'glove 1))
(define shield (ITEM "盾" 10 'shield 'shield 0))
(define chain-mail (ITEM "鎖かたびら" 40 'armor 'armor 1))
;(define S3 (CARD "♠3" 'S 3 'SHOP 'mes-s3 '() `(,magic-gloves ,shield ,chain-mail) #f #t #f))

(define horse (ITEM "馬" 40 'horse 'speed-up 3))
;(define S4 (CARD "♠4" 'S 4 'SHOP 'mes-s4 '() `(,horse) #f #t #f))

(define numbing-medicine(ITEM "しびれ薬" 5 'numbing 0))
(define anesthetic (ITEM "眠り薬" 5 'anesthetic 0))
(define medicinal-herb (ITEM "薬草" 3 'heal 4))
(define skill-herb (ITEM "技の薬" 15 'skill-full 0))
(define power-herb (ITEM "力の薬" 15 'power-full 0))
(define luck-herb (ITEM "ツキの薬" 15 'luck-full 0))
;(define S5 (CARD "♠5" 'S 5 'SHOP 'mes-s5 '() `(,numbing-medicine ,anesthetic ,medicinal-herb ,skill-herb ,power-herb ,luck-herb) #f #t #f))


;(define S6 (CARD "♠6" 'S 6 'SHOP 'mes-s6 #f '(acrobat performance handmagic instrument) #f #t #f))
;(define S7 (CARD "♠7" 'S 7 'SHOP 'mes-s7 '() '(lantern shovel rope lock-pick lute)  #f #t #f))
;(define S8 (CARD "♠8" 'S 8 'SHOP 'mes-s8 '() '(womans-clothing magic-clothing black-clothing)  #f #t #f))
;(define S9 (CARD "♠9" 'S 9 'SHOP 'mes-s9 '() '(cake)  #f #t #f))

;(define S10 (CARD "♠10" 'S 10 'SHOP 'mes-s10 '() '(fake-jewely)  #f #t #f))

(define iris (ENEMY "女戦士アイリス" 11 10)) ;ENEMYからITEMを削る、CARDに持たせる
;(define power-hammer (ITEM "パワーハンマー" #f 'one-hand-weapon '(0 1))) ;0は命中判定 1はダメージ
;(define SQ (CARD "♠Q" 'S 'Q 'cond-force 'mes-sq `(,iris) `(,power-hammer)   #f #t #t))

;(define SK (CARD "♠K" 'S 'K 'ROYAL-PALACE 'mes-sk '() '() #f #t #f))

(define devil-eyes (ITEM "デビルアイズ" #f 'event 0))
;(define DA (CARD "♢A" 'D 'A 'RAIGO 'mes-da '() `(,devil-eyes) #f #t #f))

(define orc1 (ENEMY "オークA" 6 5))
(define orc2 (ENEMY "オークB" 6 6))
;(define D2 (CARD "♢2" 'money-select 2 'BATTLE 'mes-d2 `(,orc1 ,orc2) '() 6 #t #t)) ;goldはCARDのITEMに

;(define D3 (CARD "♢3" 'D 3 'WORK 'mes-d3 '() '() 9 #t #f))

(define guard1 (ENEMY "警備兵A" 10 11))
(define guard2 (ENEMY "警備兵B" 9 10))
(define guard3 (ENEMY "警備兵C" 10 10))
;(define D4 (CARD "♢4" 'cond-force 4 'GUARD 'mes-d4 `(,guard1 ,guard2 ,guard3) '() 12 #t #t))

;(define D5 (CARD "♢5" 'D 5 'THEATER 'mes-d5 '() '() 15 #t #t))

(define pickpocketer (ENEMY "スリ" 7 7))
;(define D6 (CARD "♢6" 'D 6 'PICKPOCKET 'mes-d6 `(,pickpocketer) '() 18 #t #t))

(define mouse1 (ENEMY "大ねずみA" 2 5))
(define mouse2 (ENEMY "大ねずみB" 1 4))
(define mouse3 (ENEMY "大ねずみC" 1 4))
(define mouse4 (ENEMY "大ねずみD" 10 5))
;(define D7 (CARD "♢7" 'select  'BATTLE #f 'mes-d7 `(,mouse1 ,mouse2 ,mouse3 ,mouse4) '() 21 #t #t)) ;悪臭ステータス追加

;(define D8 (CARD "♢8" 'D 8 'PICKPOCKET 'mes-d7 '() '() 24 #t #t))
;(define D9 (CARD "♢9" 'D 9 'BATTLE 'mes-d8 '(monster-fish) '() 27 #t #t))
;(define D10 (CARD "♢10" 'D 10 'BATTLE 'mes-d9 '(oltergeist) '() 30 #t #t))

(define poison-dagger (ITEM "毒の短剣" #f 'poison-weapon '(0 0)))
;(define DQ (CARD "♢Q" 'D 'Q 'BATTLE 'mes-dq '(assassin) '(poison-dagger) 0 #t #t))

(define porcupine (ENEMY "棘棘獣" 5 7))
(define wolf (ENEMY "狼" 7 6))
(define panther (ENEMY "黒豹" 8 7))
(define dragger (ENEMY "ドラガー" 9 10))
(define cyclops (ENEMY "サイクロプス" 10 10))
(define hole-devil (ENEMY "穴悪魔" 12 15))
;(define DK (CARD "♢K" 'D 'K 'ARENA 'mes-dk `(,porcupine ,wolf ,panther ,dragger ,cyclops ,hole-devil) '() 0 #t #t))



;print関連;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define suit '("S" "D" "H" "C"))
(define num '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"))

(define *map-zero* (apply append '(
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 1 1 1 0 0)
                     (0 0 1 2 1 0 0)
                     (0 0 1 1 1 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     )))

;プレイヤー配置マップ空状態
(define *player-zero* (apply append '(
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     (0 0 0 0 0 0 0)
                     )))

;元マップに対して字札・絵札・JOKを配置する
(define (narabi zihuda q-to-k map-zero new-list)
  (if (null? map-zero)
     (reverse new-list)
      (cond ((= (car map-zero) 0)  (narabi (cdr zihuda) q-to-k (cdr map-zero) (cons (car zihuda) new-list)))
            ((= (car map-zero) 1) (narabi zihuda (cdr q-to-k) (cdr map-zero) (list (car q-to-k) new-list)))
            (else (narabi zihuda q-to-k (cdr map-zero) (list 'JOK new-list))))))

;字札・絵札・JOKを配置したマップ
;(define *map* (narabi zihuda q-to-k *map-zero* '()))





;マップ移動関連;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;キー入力でWORLD-COORDを変更する
(define (go-direct direct w) ;新たなwを返す予定 COORDとplayer-mapを更新する
  (let ((current ;PLAYER構造体から座標(INT)を束縛
                  (list-ref (WORLD-COORD w) (car (WORLD-PHASE w))))) ;現在のPLYAER構造体を返す
    (cond ((and (string=? direct "r") (ue? current)) (main-eval (change-coord direct w -7)))
          ((and (string=? direct "d") (hidari? current)) (main-eval (change-coord direct w -1)))
          ((and (string=? direct "c") (sita? current)) (main-eval (change-coord direct w 7)))
          ((and (string=? direct "f") (migi? current)) (main-eval (change-coord direct w 1)))
          (else (main-read w)))))


;整形後のPlayers配置マップを作る関数、これで移動後にworldを作り直す
(define (make-players-map new-coord)
  (split-list (map align-string (map (lambda (x) (number->string x)) (put-player *player-zero* 1 new-coord)))))


;バトル関係;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;敵ごとにランダムで味方へ攻撃対象を決める(IndexのListで)
(define (random-list e-num p-num e-attack-list)
	(if (= 0 e-num) (reverse e-attack-list)
		(random-list (- e-num 1) p-num (cons (random 1 (+ 1 p-num)) e-attack-list))))

(define (battle-read world arg)
                 (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) world))
         (match-let (((CARD NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP) SA)) ;現在のカード
                                                             ; (list-ref test-zihuda-list (- (list-ref COORD (list-ref PHASE 0)) 1))))
                  ; (let* 
                      ;   (c-enemy ;(if STATUS;運試しに勝ったか?
                                     ; (case (list-ref (list-ref FIRST 2) 1);勝った場合 ここは大幅に書き換えないと駄目
                                     ;   ((SKILLP) (ENEMY ENAME (+ ESKILLP (list-ref (list-ref FIRST 2) 2)))))
                                   ;   ENEMY)));負けてたらそのまま
                     (display (format "~%~aとの戦闘だ!" (if (< 1 (length ENEMY)) "まもののむれ" (ENEMY-NAME (car ENEMY))))) (newline)
                     (battle-read2 (WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN)
                     ))))


(define (input-command player enemy numlist)
  (newline)
  (cond ((null? player) (reverse numlist))
        (else
     (display (format "~aはどれと戦う?[1]〜[~a]~%" (PLAYER-NAME (car player)) (length enemy))) 
      (let ((answer (string->number (read-line))))
        (cond  ((or ((compose not number?) answer) (> answer (length enemy)) (> 1 answer))
                (input-command player enemy numlist))
               (else (input-command (cdr player) enemy (cons answer numlist))))))))

(define (battle-read2 world)
          (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) world))
           (match-let (((CARD NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP) SA)) ;現在のカード
                      ;  (list-ref test-zihuda-list (- (list-ref COORD (list-ref PHASE 0)) 1))))
             (let ((c-player (list-ref PLAYERS (car PHASE))))
  #; (when  (symbol? FIRST)
         (if  (symbol=? 'BATTLE-CAN-SURRENDER FIRST)
         (surrender? world enemy) (void)))
  #; (when (symbol? SECOND)
         (if (symbol=?  'BATTLE-CAN-SURRENDER SECOND)
         (surrender? world enemy) (void)))
   (battle-start world)))))

(define (surrender? world)
           (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) world))
           (match-let (((CARD NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP) ;現在のカード
                          (list-ref test-zihuda-list (- (list-ref COORD (list-ref PHASE 0)) 1))))
              (display "降参する?[y/n]") (newline) ;降参オプション有効なとき表示
             (let ((kousan-answer (read-line)))
               (cond ((string=? "y" kousan-answer) (display "降参"));((hash-ref jack-table 'SURRENDER) SECOND world)) ;戦闘中に降参する 未実装
                    ((not (string=? "y" kousan-answer)) (battle-start world ENEMIES))
                    (else (void)))))))

(define (battle-start world)
            (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) world))
              (match-let (((CARD NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP) SA)) ;現在のカード
                                                             ; (list-ref test-zihuda-list (- (list-ref COORD (list-ref PHASE 0)) 1))))
             (let ((c-player (list-ref PLAYERS (car PHASE))))
               (newline)
                      (for-each display (map (match-lambda (`(,num ,name . ,hit)
                                              (format "[~a][~a HIT:~a]~%" num name (car hit))))
                                                 (enumerate (map (lambda (x) `(,(ENEMY-NAME x) ,(ENEMY-HITP x))) ENEMIES) 1)))
               (newline)
                      (for-each display (map (match-lambda (`(,name ,hit ,skill)
                                              (format "[~a HIT:~a SKILL:~a]~%" name hit skill)))
                                (map (lambda (x) `(,(PLAYER-NAME x) ,(car (PLAYER-HITP x)) ,(car (PLAYER-SKILLP x))))
                                     (list-ref PLAYERS (car PHASE)))))
              (let ((command-list (input-command (list-ref PLAYERS (car PHASE)) ENEMIES '())))
                    (battle-eval world command-list))))))




;;;;バトルEval部分

;戦闘の種類別判定
(define (taiman car-player enemy e-count)
  (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) car-player))
    (match-let (((ENEMY E-NAME E-SKILLP E-HITP) (list-ref enemy (- e-count 1))))
      (let ((p-attack (+ (car SKILLP) (dice))) (e-attack (+ E-SKILLP (dice))))
        (cond ((= p-attack e-attack) (values 'battle-gokaku NAME E-NAME 0 0))
              ((> p-attack e-attack) (values 'battle-yusei NAME E-NAME 0 -2))
              (else (values 'battle-ressei NAME E-NAME -2 0)))))))

(define (bousen car-player enemy e-count)
  (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) car-player))
    (match-let (((ENEMY E-NAME E-SKILLP E-HITP) (list-ref enemy (- e-count 1))))
      (let ((p-attack (+ (car SKILLP) (dice))) (e-attack (+ E-SKILLP (dice))))
        (cond ((>= p-attack e-attack) (values 'battle-kawasi NAME E-NAME 0 0))
              (else (values 'battle-ressei NAME E-NAME -2 0)))))))

(define (ippouteki car-player enemy e-count)
  (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) car-player))
    (match-let (((ENEMY E-NAME E-SKILLP E-HITP) (list-ref enemy (- e-count 1))))
      (let ((p-attack (+ (car SKILLP) (dice))) (e-attack (+ E-SKILLP (dice))))
        (cond ((<= p-attack e-attack) (values 'battle-kawasare NAME E-NAME 0 0))
              (else (values 'battle-yusei NAME E-NAME 0 -2)))))))

(define (battle-zero car-player enemy car-command-list enemy-attack-list p-count e-count damage-list)
  (cond ((null? enemy-attack-list) (reverse damage-list))
        ((and (= e-count car-command-list) (= p-count (car enemy-attack-list)));タイマン通常戦闘
          (battle-zero car-player enemy car-command-list (cdr enemy-attack-list) p-count (+ 1 e-count)
           (cons (let-values (((mes name e-name p-damage e-damage) (taiman car-player enemy e-count)))
                   `(,mes ,name ,e-name ,p-damage ,e-damage)) damage-list)))
         ((= p-count (car enemy-attack-list));敵だけがこちらを攻撃
         (battle-zero car-player enemy car-command-list (cdr enemy-attack-list) p-count (+ 1 e-count)
           (cons (let-values (((mes name e-name p-damage e-damage) (bousen car-player enemy e-count)))
                   `(,mes ,name ,e-name ,p-damage ,e-damage)) damage-list)))
         ((and (not (= p-count (car enemy-attack-list))) (= e-count car-command-list));こちらだけ攻撃
          (battle-zero car-player enemy car-command-list (cdr enemy-attack-list) p-count (+ 1 e-count)
           (cons (let-values (((mes name e-name p-damage e-damage) (ippouteki car-player enemy e-count)))
                   `(,mes ,name ,e-name ,p-damage ,e-damage)) damage-list)))
         (else  (battle-zero car-player enemy car-command-list (cdr enemy-attack-list)  p-count (+ 1 e-count);どちらも狙ってない
           (cons (let-values (((mes name e-name p-damage e-damage) (values 'battle-nasi "nasi" "nasi" 0 0)))
                   `(,mes ,name ,e-name ,p-damage ,e-damage)) damage-list)))))


(define (battle-map players enemies command-list enemy-attack-list p-count damage-lists);player enemyはそれぞれリストのまま
  (if (null? command-list) (reverse damage-lists)
      (battle-map (cdr players) enemies (cdr command-list) enemy-attack-list (+ 1 p-count)
                  (cons (battle-zero (car players) enemies (car command-list) enemy-attack-list p-count 1 '()) damage-lists))))


;ダメージを適用したPlayerインスタンスのリストを返す単体
(define (damage-apply-player-zero car-player car-battle-result-list)
  (let ((player-total (foldl (lambda (y x) (+ x y)) 0 (map (lambda (z) (list-ref z 3)) car-battle-result-list))))
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) car-player))
        (PLAYER NAME SKILLP (cons (+ player-total (car HITP)) (cdr HITP)) LUCKP EQUIP GOLD ITEMS SPECIAL STATUS))));PLAYERインスタンス
;↑のマップ版
(define (damage-apply-player-map player battle-result-list new-players)
  (if (null? player) (reverse new-players)
      (damage-apply-player-map (cdr player) (cdr battle-result-list)
                               (cons (damage-apply-player-zero (car player) (car battle-result-list)) new-players))))

;ダメージを利用したEnemyインスタンスのリストを返す単体版
(define (damage-apply-enemy-zero car-enemy car-battle-result-list)
  (let ((enemy-total (foldl (lambda (y x) (+ x y)) 0 (map (lambda (z) (list-ref z 4)) car-battle-result-list))));縦貫通
    (match-let (((ENEMY ENAME ESKILLP EHITP) car-enemy))
      (ENEMY ENAME ESKILLP (+ enemy-total EHITP)))));ENEMYインスタンス

;↑のマップ版
(define (damage-apply-enemy-map enemy battle-result-listv new-enemies)
  (if (null? enemy) (reverse new-enemies)
       (damage-apply-enemy-map (cdr enemy) (cdr battle-result-listv)
                               (cons (damage-apply-enemy-zero (car enemy) (car battle-result-listv)) new-enemies))))
  

(define (battle-eval world command-list) ;playerは`(,SJ ,DJ ,HJ ,CJ)
  (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) world))
		(let* ((c-players (list-ref PLAYERS (car PHASE)))
                       (enemy-attack-list (random-list (length ENEMIES) (length c-players) '())) ;ex (1 1 2)
                        (battle-result-list
                         (battle-map c-players ENEMIES command-list enemy-attack-list 1 '())))
                    (let((new-players (damage-apply-player-map c-players battle-result-list '()))       
                         (new-enemies (damage-apply-enemy-map ENEMIES (apply map list battle-result-list) '())))
                      (battle-print new-players new-enemies world (flat-list battle-result-list '()))))))


(define (battle-print new-players new-enemies world battle-result-flat-list)
   (cond ((null? battle-result-flat-list)  (battle-loop new-players new-enemies world))
        (else (begin ((hash-ref jack-table 'battle-mes-print) (car battle-result-flat-list)) (sleep 0.3)
                     (battle-print new-players new-enemies world (cdr battle-result-flat-list))))))

(define battle-mes-print
  (match-lambda (`(,mes ,p-name ,e-name ,p-damage ,e-damage)
                 (case mes
                   ((battle-gokaku)
                    (display (format "~aと~aは互角の勝負!(゚A゚;)~%" p-name e-name)))
                   ((battle-yusei)
                    (display (format "~aは~aに[~a]ダメージを与えた(^o^)~%" p-name e-name (- e-damage))))
                   ((battle-ressei)
                    (display (format "~aは~aから[~a]ダメージを受けた(-_-;)~%" p-name e-name (- p-damage))))
                   ((battle-kawasi)
                    (display (format "~aは~aの攻撃をかわした(^o^)~%" p-name e-name)))
                   ((battle-kawasare)
                    (display (format "~aは~aに攻撃をかわされた(-_-;)~%" p-name e-name)))
                   ((battle-nasi) (void))))))

(hash-set! jack-table 'battle-mes-print battle-mes-print)

(define (battle-loop players enemies world);このworldはまだ古いWORLD players enemiesはHP変更後
  (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) world))
    (let ((new-players (filter (lambda (x) (< 0 (car (PLAYER-HITP x)))) players))
          (new-enemies (filter (lambda (x) (< 0 (ENEMY-HITP x))) enemies)))
      (cond ((null? new-players) (display "to game-over"))
            ((null? new-enemies) (display "to main-read"))
          (else (battle-read2 (WORLD (list-set PLAYERS (car PHASE) new-players) new-enemies MAPLIST SMAP PMAP PHASE COORD WIN) ))))))





;汎用ユーティリティ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;多次元リストを1次元リストに
(define (flat-list lst new-list)
  (if (null? lst) (reverse new-list)
      (flat-list (cdr lst) (append
      (let loop ((lst2 (car lst)) (new-list0 '()))
        (if (null? lst2) new-list0
            (loop (cdr lst2) (cons (car lst2) new-list0))))
            new-list))))





;print関連関数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;リストを7つの要素ごとに分割する
(define (split-list lst)
  (if (null? lst)
      '()
      (cons (take lst 7) (split-list (drop lst 7)))))

;表示桁数揃える関数 全角版
(define (align-string string)
      (cond
         ((string=? "0" string) "_____")
                     ((= 2 (string-length string)) (string-append string "___" ))
                     ((= 3 (string-length string)) (string-append string "__" ))
                     ((= 1 (string-length string)) (string-append string "____" ))))

;カードマップとPlayersマップと行ごとに交互に合体させて7要素ごとに整形
(define (display-map map players combine)
  (if (null? map)
      (display-lines (split-list (flatten (reverse combine))))
      (display-map (cdr map) (cdr players) (cons (list (car map) (car players)) combine))))



;ユーティリティ系関数;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;プレイヤー配置関数 多人数プレイ対応
(define (put-player map num players)
  (if (null? players)
      map
      (put-player (list-set map (car players) num) (+ 1 num) (cdr players))))

;何に使うんだっけ？
(define (remove-empty-lists lst)
  (cond
    [(empty? lst) empty] ; リストが空なら空を返す
    [(empty? (first lst)) (remove-empty-lists (rest lst))] ; リストの先頭が空なら削除して再帰
    [(list? (first lst)) (cons (remove-empty-lists (first lst)) ; リストの先頭がリストなら再帰的に削除して結合
                               (remove-empty-lists (rest lst)))]
    [else (cons (first lst) (remove-empty-lists (rest lst)))] ; それ以外はそのまま結合
  ))


;PHASE用Circular関数
(define (circular lst)
  (flatten (cons (cdr lst) (car lst))))

;判定用サイコロ関数
(define (dice)
  (+ (+ 1 (random 6)) (+ 1 (random 6))))
   
;LIST内の特定の要素の場所を返す
(define (list-index2 num lst count)
  (cond ((null? lst) #f)
         ((= num (car lst)) count)
         (else (list-index2 num (cdr lst) (+ 1 count)))))


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


;into-world plyaer->world 後でUtilへ移動
;PLAYERをWORLDに入れる
(define (into-world c-player world)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) world))
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
    (let ((can-equip-list (filter (lambda (x) (string=? kind (ITEM-KIND x))) ITEMS)));Kind引数にマッチするものをアイテムからフィルター
         (cond ((null? can-equip-list) (displayln "ねえよ!") (equip-change? c-player));対応物が無かったら装備するか?へ戻す
               (else 
    (let ((plus-num-list (map (lambda (num item) (cons num item)) (iota (length can-equip-list) 1 1) can-equip-list)));番号をリストにくっつける
    (for-each display (map (lambda (x) (format "[~a] ~a~%") (car x) (cdr x)) plus-num-list)) ;Noと装備候補表示
    (let ((answer (string->number (read-line))))
         (cond ((> answer (length can-equip-list)) equip-change c-player kind);答えがリスト以上だったらやり直し
               ((<= answer 0) (equip-change? c-player));0だったら装備するか?に戻す
               (else
                   (let* ((new-player (to-item-list c-player kind));現在装備しているものをItemに戻す
                          (new-equip (list-ref can-equip-list (- answer 1)));選んだアイテムをEquipにコピー
                          (new-items (delete new-equip ITEM)));Equipに指定したアイテムをItemから削除
                        (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) new-player))
                                   (PLAYER NAME SKILLP HITP LUCKP (cons new-equip EQUIP) GOLD new-items SPECIAL STATUS))))))))))))

;現在装備している装備をItemに戻す関数
(define (to-item-list player kind) ;player->player
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) player))
               (let* ((new-equip (filter (lambda (x) (not (string=? kind x))) EQUIP))
                                 (new-item (filter (lambda (x) (string=? kind x) EQUIP))))
                             (PLAYER NAME SKILLP HITP LUCKP new-equip GOLD (cons new-item ITEM) SPECIAL STATUS))))


(define (inner-luck? c-player)
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
               (if (> (car LUCKP) (dice)) #t #f)))
          
(define (change-luck c-player num)
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
               (let ((new-luckp (cond ((> (+ (car LUCKP) 1) (cdr LUCKP)) (cdr LUCKP))
                                      (else (+ (car LUCKP) 1)))))
               (PLAYER NAME SKILLP HITP (cons new-luckp (cdr LUCKP)) EQUIP GOLD ITEMS SPECIAL STATUS))))
           
(define (change-gold c-player num)
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
               (PLAYER NAME SKILLP HITP LUCKP EQUIP (+ GOLD num) ITEMS SPECIAL STATUS)))

(define (change-hitp c-player num)
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
               (let ((new-hitp (cond ((> (+ (car HITP) 1) (cdr HITP)) (cdr HITP))
                                      (else (+ (car HITP) 1)))))
               (PLAYER NAME SKILLP (cons new-hitp (cdr HITP)) LUCKP EQUIP GOLD ITEMS SPECIAL STATUS))))
           
(define (change-skillp c-player num)
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))
               (let ((new-skillp (cond ((> (+ (car SKILLP) 1) (cdr SKILLP)) (cdr SKILLP))
                                      (else (+ (car SKILLP) 1)))))
               (PLAYER NAME (cons new-skillp (cdr SKILLP)) HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS))))
           

(define (change-coord W num)
                    (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
                               (WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE (list-set COORD (car PHASE) num) WIN)))

;リストからアイテムを除去
(define (delete-item-list item lst new-lst)
    (cond ((null? lst) new-lst)
          ((equal? item (car lst)) (delete-item-list item (cdr lst) new-lst))
          (else (delete-item-list item (cdr lst) (cons (car lst) new-lst)))))
                                    

(define (mes-return NAME ENAME c-arg count) ;(((0 0 0 -3 0 #f)
  (if (not (zero? (car c-arg)))
      (cond ((= count 0) (values NAME "技術点"  (car c-arg)))
            ((= count 1) (values NAME "体力点" (car c-arg)))
            ((= count 2) (values NAME "幸運点" (car c-arg)))
            ((= count 3) (values ENAME "技術点" (car c-arg)))
            ((= count 4) (values ENAME "体力点" (car c-arg)))
            (else #f))
      (mes-return NAME ENAME (cdr c-arg) (+ 1 count))))


;PLYAERのステータスを変更する関数
(define (change-status PLAYERS PHASE arg) ; ->PLAYERS
  (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) (list-ref PLAYERS PHASE)))
   (let ((new-player-status
    (case (list-ref arg 1)
      ((HITP) (PLAYER NAME SKILLP
                      (cons (+ (car HITP) (list-ref arg 2)) (cdr HITP))
                      LUCKP EQUIP GOLD ITEMS SPECIAL STATUS)))))
     (list-set PLAYERS PHASE new-player-status))))
  

;座標を無理やり変更する関数(CARD-FIRSTの失敗時の判定の結果)
(define (force-coord COORD PHASE int)
  (list-set COORD (car PHASE) int))









;FOLDで使うクロージャ系;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;選択肢で脱出用の関数
(define next-player (lambda (W A)
          (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (main-read (WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP (circular PHASE) COORD WIN)))))
(define (change-coord-main direct w num)
  (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) w))
    (let* ((new-coord (list-set COORD PHASE (+ (list-ref COORD PHASE) num)))
           (new-pmap (make-players-map new-coord)))
      (WORLD PLAYERS MAPLIST SMAP new-pmap PHASE new-coord WIN))))


;select 戦うかどうか選択のクロージャ 引数はworld
(define select (lambda (W A) ;`(#f ,next-player)
                 (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
                   (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                         (c-card SA)) ;(list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
                     (match-let (((CARD NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP) SA))
                     (display "受けるか?") (newline)
                     (let ((answer (read-line))) 
                       (if (not (string=? "y" answer)) ;戦闘を受ける場合
                           ((list-ref A 1) W A)
                           W)))))))
  
  
;PLAYERの任意のステタースが条件を満たしているかチェック
(define satisfy-status? (lambda (W A)
     (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
          (let ((c-player (list-ref PLAYERS (list-ref PHASE 0))))
               (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player))            
               (cond ((satisfy-status? c-player (list-ref A 0)) W) ;満たしていれば素通り
                     (else ((list-ref A 1) W A)))))))) ;ここではA-LISTにnext-playerを入れておく


;イベントが発生するのに必要なアイテムがあるかどうか調べる               
(define satisfy-item (lambda (W A) ; `((necessary-items) `luck?)
    (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                  (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
              (if (satisfy-item? (list-ref A 0) (PLAYER-ITEMS (car c-player)))
                  ((hash-ref jack-table (list-ref A 1)) W A)
                  W))))) ;アイテムがなかった場合(多分)素通り

                       
(define luck? (lambda (W A) ;W Aはsatisfy-itemに入ってきてる引数そのまま
        (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                  (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
              (display "運試しをするかね?") (newline)
              (let ((answer (read-line)))
                   (if (string=? "y") (luck W A) ;luckで変更後のWORLD構造体を返すようにする
                       W)))))) ;しないなら素通しでW Aを返す


(define (luck W A) ;W Aはsatisfy-itemに入ってきてる引数そのまま
           (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (let ((c-player (car (list-ref PLAYERS (list-ref PHASE 0))));今のPLAYERインスタンス
                  (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
              (display "運試しの結果は・・") (newline)
              (if (> (car (PLAYER-LUCKP c-player)) (dice)) 
                  (luck-result W (car (list-ref A 2))) ;((0 0 0) (-3 0) #f)
                  (luck-result W (cadr (list-ref A 2))))))) ;((0  -2 0) (0 0) 24)


(define (luck-result W A) ;satisfy-itemに返す
            (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (let* ((c-players (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                   (c-player (car c-players))
                  (c-card (list-ref COORD (list-ref PHASE 0)));今のCARDインスタンス
                  (p-skill (car (car A))) (p-hit (cadr (car A))) (p-luck (caddr (car A)))
                  (e-skill (car (cadr A))) (e-hit (cadr (cadr A))))
              (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL STATUS) c-player)
                         ((ENEMY ENAME ESKILLP EHITP) (car ENEMIES)))
              (let ((new-player (PLAYER NAME (cons (+ (car SKILLP) p-skill) (cdr SKILLP))
                                        (cons (+ (car HITP) p-hit) (cdr HITP))
                                        (cons (+ (car LUCKP) p-luck) (cdr LUCKP)) EQUIP GOLD ITEMS SPECIAL STATUS))
                     (new-enemy (ENEMY ENAME (+ ESKILLP e-skill) (+ EHITP e-hit))))
                 (let-values (((name status num)  (mes-return NAME ENAME (flatten A) 0)))
                 (display (format "~aは~aが~aされた" name status num))) 
                 (WORLD (list-set PLAYERS (car PHASE) (list-set c-players 0 new-player))
                        (list-set ENEMIES 0 new-enemy)
                        MAPLIST SMAP PMAP PHASE COORD WIN)))))) ;LuckでのCOORD変更から続き




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



;main関数たち;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;メインREAD マップの移動入力
(define (main-read w)
  (display-map (WORLD-SMAP w) (WORLD-PMAP w) '())
  (newline)
  (display (format (hash-ref jack-table 'mes-read) (list-ref (WORLD-PLAYERS w) (car (WORLD-PHASE w)))))
  (let ((direct (read-line)))
    (cond ((member direct  '("r" "d" "c" "f")) ((hash-ref jack-table 'direct) direct w))
          (else (main-read w)))))
  
;メインEVAL 移動後イベント発生
(define (main-eval w)
  (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) w))
  (display-map SMAP PMAP '()) (newline)
  (let ((c-card (list-ref test-zihuda-list (list-ref (WORLD-COORD w) (car (WORLD-PHASE w))))));現在いるカード
  ;(if (list-ref MAPLIST (list-ref COORD PHASE)) ;CARD-ONが#fなら何も起きないので次のプレイヤーへ
      (display "to main-loop"))))
     ; (main-loop (WORLD PLAYERS MAPLIST SMAP PMAP PHASE COORD WIN)))))) ;main-loopで勝利条件の判定








;test;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PLAYERインスタンス
(define SJ (PLAYER "勇者スペードのジャック" '(11 . 11) '(22 . 22) '(9 . 9) `(,sword ) 20 `(,numbing-medicine ,wine) #f 'win-sj))
(define DJ (PLAYER "恋人ダイヤのジャック" '(10 . 10) '(20 . 20) '(10 . 10) '(sword) 20 '() 'skill-miracle 'win-dj))
(define HJ (PLAYER "弟子ハートのジャック" '(9 . 9) '(18 . 18) '(11 . 11) '(sword) 20 '() 'skill-ilusion 'win-hj))
(define CJ (PLAYER "悪魔クラブのジャック" '(10 . 10) '(20 . 20) '(9 . 9) '(sword) 20 '() 'skill-recover 'win-cj))

;WORLD構造体のプレイヤー座標リスト
(define players '(0 10 9 10))

;WORLD構造体のプレイヤー順番リスト（4人プレイ用）
(define phase-list '(0 1 2 3))


(define (test-eval world card)
  (foldl (lambda (func arg initial) (func initial arg)) world (CARD-FLIST card) (CARD-ALIST card)))


;整形後のPlayers配置マップを束縛(初期値)
(define players-map (split-list (map align-string (map (lambda (x) (number->string x)) (put-player *player-zero* 1 players)))))


(define test-list
 (flat-list (battle-map `(,SJ ,DJ ,HJ)
                         `(,mouse1 ,mouse2 ,mouse3 ,mouse4) '(1 2 3) '(1 2 3 2) 1  '()) '()))



(define SA (CARD "♠A" `(,select ,satisfy-item ,battle-read) `(() ;selectには引数不要
            ((,numbing-medicine ,??) luck? (((0 0 0) (0 -3) #f) ((0  -2 0) (0 0) 24)));satisfy-item用引数
           '()) ;battleには引数不要
                 'mes-sa `(,zakura) '(rune-blade) 0 #t))

;テスト用インスタンス類

(define world (WORLD `((,SJ ,DJ ,HJ ,CJ)()) `(,zakura) '() test-string-list test-zihuda-list phase-list players #f))

(test-eval world SA)


(define test-zihuda-list '())
  ;`(,SA ,S2)); ,S3 ,S4 ,S5 ,S6 ,S7 ,S8 ,S9 ,S10
              ;             ,DA ,D2 ,D3 ,D4 ,D5 ,D6 ,D7 ,D8 ,D9 ,D10 ,DQ))


(define test-string-list  (flatten (map (lambda (x) (list (CARD-NAME x))) test-zihuda-list)))


