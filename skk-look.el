;;; skk-look.el --- UNIX look command interface for SKK
;; Copyright (C) 1998, 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-look.el,v 1.5.2.4.2.2 1999/12/05 05:59:26 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/12/05 05:59:26 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; <How to work>
;; .skk か .emacs で `skk-use-look' を t にセットしてこれを評価して下さい。その
;; 後 skk-mode を立ち上げるか、M-x skk-restart すると、SKK abbrev モードで、
;; UNIX look コマンドを利用した英文字 + アスタリスクの変換ができるようになりま
;; す。こんな感じです。
;;
;; ▽confere* (SPC)
;; ---> ▼conference
;;
;; 確定すると、`confere*' を見出し語、`conference' を候補とするエントリが個人辞
;; 書に追加されます。このようなエントリを追加したくない場合は、
;; skk.el のユーザー変数、`skk-search-excluding-word-pattern-function' を適切に
;; 設定することで、これを実現することができます。詳しくは、
;; `skk-search-excluding-word-pattern-function' のドキュメントをご覧下さい。
;;
;; `skk-look-recursive-search' の値を non-nil にすると、look が見つけた英単語を
;; 見出し語にして、再帰的に SKK 辞書内を検索することができます。例えば、いずれか
;; の SKK 辞書に
;;
;;  abstract /アブストラクト/抽象/
;;  abstraction /アブストラクション/
;;
;; というエントリがある場合、
;;
;;  ▽abs* (SPC)
;;
;;  ---> ▼abstract (SPC) -> ▼アブストラクト (SPC) -> ▼抽象 (SPC)
;;       -> ▼abstraction (SPC) -> ▼アブストラクション
;;
;; のように英単語 + その英単語を見出し語にした候補の「セット」を変換結果として出
;; 力することができます。この際、`skk-look-expanded-word-only' の値が non-nil で
;; あれば、再帰検索に成功した英単語の「セット」だけ (再帰検索で検出されなかった
;; 英単語は無視する) を出力することができます。
;;
;; abbrev モードで補完を行なうと、個人辞書を検索し尽した後で、look コマンドによる
;; 英単語補完を行ないます。例えば、こんな感じに動作します。
;;
;;  ▽confe (TAB)
;;  ---> ▽conference
;;
;; 動作確認を行なった look は、Slackware 3.5 に入っていた、man page に
;; `BSD Experimental June 14, 1993' と記載のあるもの (バージョン情報がない) にて
;; 行なっています。オプションの指定などが異なる look があれば、ご一報下さい。よろ
;; しくお願いいたします。

;; <Dictionary>
;; ftp://ftp.u-aizu.ac.jp:/pub/SciEng/nihongo/ftp.cc.monash.edu.au/
;; に置いてある edict を利用すると手軽に英和辞書ができます。
;; 
;;   % jgawk -f skk-10/lisp/look/edict2skk.awk edict > temp
;;   % skkdic-expr temp | skkdic-sort > SKK-JISYO.E2J
;;   % rm temp
;;
;; できた SKK-JISYO.E2J の利用方法は色々ありますが、
;;
;;   % skkdic-expr SKK-JISYO.E2J + /usr/local/share/skk/SKK-JISYO.L | skkdic-sort > SKK-JISYO.L
;;
;; などとして、SKK-JISYO.L とマージして使うのが手軽です。

;; <Motivation>
;; このプログラムは、eWnn for Linux/FreeBSD の広告に類似の機能紹介があったのを見
;; て、「こんな機能なら SKK 上にすぐインプリメントできるさ」と思うとたまらくなっ
;; て書いてしまいました。eWnn に負けるな、SKK!
;;
;; 昔、Seiichi Namba <sn@asahi-net.email.ne.jp> さんと一緒に Emacs Lisp で
;; look interface を書いたことがあるのですが、今回はその際の経験を生かすことができ
;; ました。難波さんに感謝いたします。

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

(and skk-look-command
     (null (member '(skk-look) skk-search-prog-list))
     (let ((pl skk-search-prog-list)
	   (n 0) dic mark )
       (while pl
	 (setq dic (car pl))
	 (if (memq (nth 1 dic) '(skk-jisyo skk-rdbms-private-jisyo-table))
	     (setq mark n
		   pl nil)
	   (setq pl (cdr pl)
		 n (1+ n) )))
       (skk-splice-in skk-search-prog-list (1+ mark)
		      '((skk-look)) )))

;; program
;;;###autoload
(defun skk-look ()
  ;; UNIX look コマンドを利用した変換を行なう。
  ;; SKK abbrev モードにて、英文字 + アスタリスクで uncompleted spelling を指定
  ;; する。
  (and skk-abbrev-mode
       (eq (skk-str-ref skk-henkan-key (1- (length skk-henkan-key))) ?*)
       (let ((args (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	     v )
	 (setq v (skk-look-1 args))
	 (if (not skk-look-recursive-search)
	     v
	   (let (skk-henkan-key v2 v3)
	     (while v
	       (let ((skk-current-search-prog-list
		      (delete '(skk-look) (copy-sequence skk-search-prog-list)) ))
		 (setq skk-henkan-key (car v))
		 (while skk-current-search-prog-list
		   (setq v3 (skk-search)
			 v2 (if (not skk-look-expanded-word-only)
				(skk-nunion v2 (cons (car v) v3))
			      (if v3
				  (skk-nunion v2 (cons (car v) v3))
				v2 )))))
	       (setq v (cdr v)) )
	     v2 )))))

(defun skk-look-1 (args)
  ;; core search engine
  (with-temp-buffer 
    (let (opt)
      (setq args (list args))
      (and skk-look-dictionary (nconc args (list skk-look-dictionary)))
      (and skk-look-dictionary-order (setq opt "d"))
      (and skk-look-ignore-case (setq opt (concat "f" opt)))
      (and skk-look-use-alternate-dictionary
	   (setq opt (concat "a" opt)) )
      (and opt (setq args (cons (concat "-" opt) args)))
      (and skk-look-termination-character
	   (setq args
		 (cons (list "-t" skk-look-termination-character) args) ))
      (and
       (= 0 (apply 'call-process skk-look-command nil t nil args))
       (> (buffer-size) 0)
       (split-string (buffer-substring-no-properties (point-min) (1- (point-max)))
		     "\n" )))))

;;;###autoload
(defun skk-look-completion ()
  (or skk-look-completion-words
      (let ((stacked skk-completion-stack))
	;; look は複数の候補を吐くので、一旦貯めておいて、一つづつ complete する。
	(setq skk-look-completion-words
	      (delete skk-completion-word (skk-look-1 skk-completion-word)) )
	(while stacked
	  (setq skk-look-completion-words
		(delete (car stacked) skk-look-completion-words)
		stacked (cdr stacked) ))))
  (prog1
      (car skk-look-completion-words)
    (setq skk-look-completion-words (cdr skk-look-completion-words)) ))

(defadvice skk-kakutei-initialize (after skk-look-ad activate)
  (setq skk-look-completion-words nil) )

(provide 'skk-look)
;;; skk-look.el ends here
