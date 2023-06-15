#lang racket



(require srfi/1)
(require racket/struct)
(require racket/match)
(require "4king-data.rkt")
(require "4king-util.rkt")
#|

;(define SA (CARD "♠A" 'SELECT ;初期Eval用キー ここではselectクロージャを呼び出す ;CARD-KIND
 ;                '(LUCK-TRY (numbing-medicine wine) (enemy SKILLP -3) (player HITP -2 24));CARD-FIRST
                 ;↑#tで続く()で必要アイテム、次の()で成功効果 最後の()で失敗効果最後の真偽はJOK(24)行きかどうか
  ;               '(LUCKP -2) ;存在した場合は降服可能、BATTLEで参照してメニューを出す CARD-SECIND
   ;              'mes-s1 (list zakura) (list rune-blade) #f #t #t))
;(NAME FLIST ALIST MES ENEMY ITEM GOLD FLIP
(define SA (CARD "♠Ａ" '(select satisfy-item battle) '(() ;selectには引数不要
            ((numbing-medicine wine) luck? (((0 0 0) (-3 0) #f) ((0  -2 0) (0 0) 24)));satisfy-item用引数
           '()) ;battleには引数不要
                 'mes-sa '(zakura) '(rune-blade) 0 #t))

;SELECT> SATISFY-ITEM(luck) > BATTLE
;ALISTの中にNoの場合の行き先を含める

;select 戦うかどうか選択のクロージャ 引数はworld
(define select (lambda (W A)
                 (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
               ;    (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                ;         (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
                     (display "受けるか?") (newline)
                     (let ((answer (read-line))) 
                       (if (string=? "y" answer) ;戦闘を受ける場合
                           W
                           (main-read (WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP (circular PHASE) COORD WIN)))))))
                       
(define satisfy-item (lambda (W A) ;アイテムのある無しで分岐するルールにする
    (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                  (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
              (if (satisfy-item? (PLAYER-ITEMS (car c-player)) (list-ref A 0)) ;Aの中身は適当
                  ((hash-ref jack-table (list-ref A 1)) W A)
                  (WORLD W A)))))) ;アイテムがなかった場合(多分)素通り
                       
(define luck? (lambda (W A) ;W Aはsatisfy-itemに入ってきてる引数そのまま
        (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (let ((c-player (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                  (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
              (display "運試しをするかね?") (newline)
              (let ((answer ((read-line))))
                   (if (string=? "y") (luck W A) ;luckで変更後のWORLD構造体を返すようにする
                       w)))))) ;しないなら素通しでW Aを返す


(define luck (lambda W A) ;W Aはsatisfy-itemに入ってきてる引数そのまま
           (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (let ((c-player (car (list-ref PLAYERS (list-ref PHASE 0))));今のPLAYERインスタンス
                  (c-card (list-ref COORD (list-ref PHASE 0))));今のCARDインスタンス
              (display "運試しの結果は・・") (newline)
              (if (> (PLAYER-LUCKP c-player) (dice)) 
                  (luck-result W (car (list-ref A 2))) ;((0 0 0) (-3 0) #f)
                  (luck-result W (cadr (list-ref A 2))))))) ;((0  -2 0) (0 0) 24)

(define luck-result (W A) ;satisfy-itemに返す
            (match-let (((WORLD PLAYERS ENEMIES MAPLIST SMAP PMAP PHASE COORD WIN) W))
            (let* ((c-players (list-ref PLAYERS (list-ref PHASE 0)));今のPLAYERインスタンス
                   (c-player (car c-players))
                  (c-card (list-ref COORD (list-ref PHASE 0)));今のCARDインスタンス
                  (p-skill (car (car A))) (p-hit (cadr (car A)) (p-luck (caddr (car A)))
                  (e-skill (car (cadr A))) (e-hit (cadr (cadr A)))))
              (match-let (((PLAYER NAME SKILLP HITP LUCKP EQUIP GOLD ITEMS SPECIAL WIN) c-player)
                         ((ENEMY ENAME ESKILLP EHITP) (car ENEMIES)))
              ((let ((new-player (PLAYER NAME (+ SKILLP p-skill) (+ HITP p-hit) (+ LUCKP p-luck) EQUIP GOLD ITEMS SPECIAL WIN))
                     (new-enemy (ENEMY ENAME (+ ESKILLP e-skill) (+ EHITP e-hit))))
                 (let-values (((name status num)  (mes-return NAME ENAME (flatten A) 0)))
                 (display (format "~aは~aが~aされた" name status num)))
                 (WORLD (list-set PLAYERS (car PHASE) (list-set c-players 0 new-player))
                        (list-set ENEMIES 0 new-enemy)
                        MAPLIST SMAP PMAP PHASE COORD WIN))))))) ;LuckでのCOORD変更から続き

(define (mes-return NAME ENAME c-arg count) ;(((0 0 0 -3 0 #f)
  (if (not (zero? (car c-arg)))
      (cond ((= count 0) (values NAME "技術点が"  (car c-arg)))
            ((= count 1) (values NAME "体力点が" (car c-arg)))
            ((= count 2) (vaules NAME "幸運点が" (car c-arg)))
            ((= count 3) (values ENAME "技術点が" (car c-arg)))
            ((= count 4) (values ENAME "体力点が" (car c-arg)))
            (else #f))
      (mes-return NAME ENAME (cdr c-arg) (+ 1 count))))

|#
