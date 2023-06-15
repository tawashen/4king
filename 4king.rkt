#lang racket


(require 2htdp/universe 2htdp/image lang/posn)
(require srfi/1)
;(require srfi/13)
(require racket/struct)
(require racket/match)
(require "4king-print.rkt")
(require "4king-util.rkt")
(require "4king-data.rkt")
(require "4king-mes.rkt")


;PLAYERインスタンス
(define SJ (PLAYER "勇者スペードのジャック" '(11 . 11) '(22 . 22) '(9 . 9) `(,sword ) 20 `(,numbing-medicine ,wine) #f 'win-sj))
(define DJ (PLAYER "恋人ダイヤのジャック" '(10 . 10) '(20 . 20) '(10 . 10) '(sword) 20 '() 'skill-miracle 'win-dj))
(define HJ (PLAYER "弟子ハートのジャック" '(9 . 9) '(18 . 18) '(11 . 11) '(sword) 20 '() 'skill-ilusion 'win-hj))
(define CJ (PLAYER "悪魔クラブのジャック" '(10 . 10) '(20 . 20) '(9 . 9) '(sword) 20 '() 'skill-recover 'win-cj))


(define players '(0 10 9 10))
(define phase-list '(0 1 2 3))

;整形後のPlayers配置マップを束縛（初期値）
(define players-map (split-list (map align-string (map (lambda (x) (number->string x)) (put-player *player-zero* 1 players))))) 






;ここにしか置けない関数
(define (go-direct direct w) ;新たなwを返す予定 COORDとplayer-mapを更新する
  (let ((current ;PLAYER構造体から座標(INT)を束縛
                  (list-ref (WORLD-COORD w) (car (WORLD-PHASE w))))) ;現在のPLYAER構造体を返す
    (cond ((and (string=? direct "r") (ue? current)) (main-eval (change-coord direct w -7)))
          ((and (string=? direct "d") (hidari? current)) (main-eval (change-coord direct w -1)))
          ((and (string=? direct "c") (sita? current)) (main-eval (change-coord direct w 7)))
          ((and (string=? direct "f") (migi? current)) (main-eval (change-coord direct w 1)))
          (else (main-read w)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;ここから関数のテスト
;select 戦うかどうか選択のクロージャ 引数はworld
(define select (lambda (W A)
                 (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
                   (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                         (c-card SA)) ;(list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
                     (match-let (((CARD NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP) SA))
                     (display "受けるか?") (newline)
                     (let ((answer (read-line))) 
                       (if (string=? "y" answer) ;戦闘を受ける場合
                           W
                           (main-read (WORLD PLAYERS ENEMY MAPLIST SMAP PMAP (circular PHASE) COORD WIN)))))))))


                       
(define satisfy-item (lambda (W A) ;アイテムのある無しで分岐するルールにする
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

(hash-set! jack-table 'luck? luck?)

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
              (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL WIN) c-player)
                         ((ENEMY ENAME ESKILLP EHITP) (car ENEMIES)))
              (let ((new-player (PLAYER NAME (cons (+ (car SKILLP) p-skill) (cdr SKILLP))
                                        (cons (+ (car HITP) p-hit) (cdr HITP))
                                        (cons (+ (car LUCKP) p-luck) (cdr LUCKP)) EQUIP GOLD ITEMS SPECIAL WIN))
                     (new-enemy (ENEMY ENAME (+ ESKILLP e-skill) (+ EHITP e-hit))))
                 (let-values (((name status num)  (mes-return NAME ENAME (flatten A) 0)))
                 (display (format "~aは~aが~aされた" name status num))) 
                 (WORLD (list-set PLAYERS (car PHASE) (list-set c-players 0 new-player))
                        (list-set ENEMIES 0 new-enemy)
                        MAPLIST SMAP PMAP PHASE COORD WIN)))))) ;LuckでのCOORD変更から続き

(define (mes-return NAME ENAME c-arg count) ;(((0 0 0 -3 0 #f)
  (if (not (zero? (car c-arg)))
      (cond ((= count 0) (values NAME "技術点"  (car c-arg)))
            ((= count 1) (values NAME "体力点" (car c-arg)))
            ((= count 2) (values NAME "幸運点" (car c-arg)))
            ((= count 3) (values ENAME "技術点" (car c-arg)))
            ((= count 4) (values ENAME "体力点" (car c-arg)))
            (else #f))
      (mes-return NAME ENAME (cdr c-arg) (+ 1 count))))




(define (test-eval world card)
  (foldl (lambda (func arg initial) (func initial arg)) world (CARD-FLIST card) (CARD-ALIST card)))





                                                 
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
;メインREAD マップの移動入力
(define (main-read w)
  (display-map (WORLD-SMAP w) (WORLD-PMAP w) '())
  (newline)
  (display (format (hash-ref jack-table 'mes-read) (list-ref (WORLD-PLAYERS w) (car (WORLD-PHASE w)))))
  (let ((direct (read-line)))
    (cond ((member direct  '("r" "d" "c" "f")) ((hash-ref jack-table 'direct) direct w))
          (else (main-read w)))))
  
;メインEVAL　移動後イベント発生
(define (main-eval w)
  (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) w))
  (display-map SMAP PMAP '()) (newline)
  (let ((c-card (list-ref test-zihuda-list (list-ref (WORLD-COORD w) (car (WORLD-PHASE w))))));現在いるカード
  ;(if (list-ref MAPLIST (list-ref COORD PHASE)) ;CARD-ONが#fなら何も起きないので次のプレイヤーへ
     

      (display "to main-loop"))))
     ; (main-loop (WORLD PLAYERS MAPLIST SMAP PMAP PHASE COORD WIN)))))) ;main-loopで勝利条件の判定

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

;PLYAERのステータスを変更する関数
(define (change-status PLAYERS PHASE arg) ; ->PLAYERS
  (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL WIN) (list-ref PLAYERS PHASE)))
   (let ((new-player-status
    (case (list-ref arg 1)
      ((HITP) (PLAYER NAME SKILLP
                      (cons (+ (car HITP) (list-ref arg 2)) (cdr HITP))
                      LUCKP EQUIP GOLD ITEMS SPECIAL WIN)))))
     (list-set PLAYERS PHASE new-player-status))))
  

;座標を無理やり変更する関数（CARD-FIRSTの失敗時の判定の結果）
(define (force-coord COORD PHASE int)
  (list-set COORD (car PHASE) int))

;BATTLE関数 一時コメントアウト

(define (battle-read world arg)
                 (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) world))
         (match-let (((CARD NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP) SA)) ;現在のカード
                                                             ; (list-ref test-zihuda-list (- (list-ref COORD (list-ref PHASE 0)) 1))))
                  ; (let* 
                      ;   (c-enemy ;(if WIN;運試しに勝ったか？
                                     ; (case (list-ref (list-ref FIRST 2) 1);勝った場合 ここは大幅に書き換えないと駄目
                                     ;   ((SKILLP) (ENEMY ENAME (+ ESKILLP (list-ref (list-ref FIRST 2) 2)))))
                                   ;   ENEMY)));負けてたらそのまま
                     (display (format "~%~aとの戦闘だ！" (if (< 1 (length ENEMY)) "まもののむれ" (ENEMY-NAME (car ENEMY))))) (newline)
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
         (surrender? world　enemy) (void)))
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
  (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL WIN) car-player))
    (match-let (((ENEMY E-NAME E-SKILLP E-HITP) (list-ref enemy (- e-count 1))))
      (let ((p-attack (+ (car SKILLP) (dice))) (e-attack (+ E-SKILLP (dice))))
        (cond ((= p-attack e-attack) (values 'battle-gokaku NAME E-NAME 0 0))
              ((> p-attack e-attack) (values 'battle-yusei NAME E-NAME 0 -2))
              (else (values 'battle-ressei NAME E-NAME -2 0)))))))

(define (bousen car-player enemy e-count)
  (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL WIN) car-player))
    (match-let (((ENEMY E-NAME E-SKILLP E-HITP) (list-ref enemy (- e-count 1))))
      (let ((p-attack (+ (car SKILLP) (dice))) (e-attack (+ E-SKILLP (dice))))
        (cond ((>= p-attack e-attack) (values 'battle-kawasi NAME E-NAME 0 0))
              (else (values 'battle-ressei NAME E-NAME -2 0)))))))

(define (ippouteki car-player enemy e-count)
  (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL WIN) car-player))
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
    (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL WIN) car-player))
        (PLAYER NAME SKILLP (cons (+ player-total (car HITP)) (cdr HITP)) LUCKP EQUIP GOLD ITEMS SPECIAL WIN))));PLAYERインスタンス
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
                    (display (format "~aと~aは互角の勝負!(ﾟＡﾟ;)~%" p-name e-name)))
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


(define test-list
 (flat-list (battle-map `(,SJ ,DJ ,HJ)
                         `(,mouse1 ,mouse2 ,mouse3 ,mouse4) '(1 2 3) '(1 2 3 2) 1  '()) '()))



(define SA (CARD "♠A" `(,select ,satisfy-item ,battle-read) `(() ;selectには引数不要
            ((,numbing-medicine ,wine) luck? (((0 0 0) (0 -3) #f) ((0  -2 0) (0 0) 24)));satisfy-item用引数
           '()) ;battleには引数不要
                 'mes-sa `(,zakura) '(rune-blade) 0 #t))

;テスト用インスタンス類

(define world (WORLD `((,SJ ,DJ ,HJ ,CJ)()) `(,zakura) '() test-string-list test-zihuda-list phase-list players #f))

(test-eval world SA)


;(battle-read world)
  

                                

                  
;;;;;;;;;;;ここまで


                     
                     
               

;(main-print world)
;(main-read world)









