;; skk-viper.el --- SKK related code for Viper
;; Copyright (C) 1996, 1997, 1998, 1999
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>, Murata Shuuichirou <mrt@astec.co.jp>
;;
;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>,
;;         Murata Shuuichirou <mrt@notwork.org>
;; Maintainer: Murata Shuuichirou <mrt@notwork.org>
;;             Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-viper.el,v 1.5.2.4.2.9 2000/04/03 08:46:22 mrt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/04/03 08:46:22 $

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

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))
(require 'viper)

;; macros and inline functions.
(defmacro skk-viper-advice-select (viper vip arg body)
  (` (if skk-viper-use-vip-prefix
	 (defadvice (, vip) (, arg) (,@ body))
       (defadvice (, viper) (, arg) (,@ body)))))

(setq skk-kana-cleanup-command-list
      (cons 
       (if skk-viper-use-vip-prefix
	   'vip-del-backward-char-in-insert
	 'viper-del-backward-char-in-insert )
       skk-kana-cleanup-command-list ))

(setq skk-use-viper t)
(save-match-data
  (or (string-match sentence-end "。？！")
      (setq sentence-end (concat "[。？！]\\|" sentence-end))))

;; cursor color support.
;; static-when を使いたいのに、 バイトコンパイル時に X window が
;; initialize されていないというエラーになる (;_;)。
;; yatex みたいに Window を開いてバイトコンパイルするようにもできるけど、
;; no-window の make コマンドを別に作らなきゃならないし、面倒だな...。
(when (skk-color-display-p)
  (if skk-use-color-cursor 
      (progn
	(defvar skk-viper-saved-cursor-color viper-insert-state-cursor-color)
	;; 恐ろしや〜、必殺のバッファローカル...。
	(make-variable-buffer-local 'viper-insert-state-cursor-color)
	;; SKK-CURSOR related.
	(defadvice skk-cursor-set-properly (around skk-viper-cursor-ad activate)
	  "vi-state のときは、SKK モードになっていてもカーソルをディフォルトにしておく。"
	  (if (static-cond ((boundp 'viper-current-state)
			    (eq viper-current-state 'vi-state))
			   ((boundp 'vip-current-state)
			    (eq vip-current-state 'vi-state)))
	      (progn
		(ad-set-arg 0 skk-cursor-default-color)
		ad-do-it )
	    ad-do-it 
	    (cond ((not skk-mode)
		   (setq viper-insert-state-cursor-color
			 skk-viper-saved-cursor-color))
		  (t
		   (setq viper-insert-state-cursor-color
			 (skk-cursor-current-color))))))

	;; cover to VIP/Viper functions.
	(let ((funcs
	       (if skk-viper-use-vip-prefix
		   '(vip-Append vip-Insert vip-insert vip-intercept-ESC-key 
				vip-open-line)
		 '(viper-Append viper-Insert viper-hide-replace-overlay 
				viper-insert viper-intercept-ESC-key 
				viper-open-line ))))
	  (while funcs
	    (eval
	     (`
	      (defadvice (, (intern (symbol-name (car funcs))))
		(after skk-viper-cursor-ad activate)
		"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
		;; skk-mode が nil か non-nil かの判定付き。
		;; CLASS は after.
		(and skk-mode (skk-cursor-set-properly)))))
	    (setq funcs (cdr funcs))))

	;; first remove skk-cursor advice.
	;;(ad-remove-advice 'skk-mode 'after 'skk-cursor)
	;; then put on new advice specially made for viper.
	;;(defadvice skk-mode (after skk-viper-ad activate)
	;;  (add-hook 'viper-post-command-hooks 'skk-set-cursor-properly 'append 'local))

	(if (boundp 'viper-insert-state-cursor-color)
	    (let ((funcs '(skk-abbrev-mode skk-jisx0208-latin-mode 
					   skk-latin-mode skk-toggle-kana)))
	      (while funcs
		(eval
		 (`
		  (defadvice (, (intern (symbol-name (car funcs))))
		    (after skk-viper-cursor-ad activate)
		    "viper-insert-state-cursor-color を SKK の入力モードのカーソル色と合わせる。"
		    (setq viper-insert-state-cursor-color (skk-cursor-current-color)))))
		(setq funcs (cdr funcs)))))

	(defadvice skk-mode (after skk-viper-cursor-ad activate)
	  "viper-insert-state-cursor-color を SKK の入力モードのカーソル色と合わせる。"
	  (setq viper-insert-state-cursor-color 
		(if skk-mode (skk-cursor-current-color)
		  skk-viper-saved-cursor-color ))
	  ;; insert mode になったら Viper 側でカーソルを変更する。
	  ;;(viper-change-cursor-color viper-insert-state-cursor-color)
	  )
	(defadvice skk-kakutei (after skk-viper-cursor-ad activate)
	  (setq viper-insert-state-cursor-color skk-cursor-hiragana-color)))))

;; vip-4 の同種の関数名は vip-read-string-with-history？
(defadvice viper-read-string-with-history (after skk-viper-ad activate)
  "次回ミニバッファに入ったときに SKK モードにならないようにする。"
  (remove-hook 'pre-command-hook 'skk-pre-command 'local)
  (skk-remove-minibuffer-setup-hook
   'skk-j-mode-on 'skk-setup-minibuffer
   (function (lambda ()
	       (add-hook 'pre-command-hook 'skk-pre-command nil 'local)))))

(if skk-use-color-cursor
    (defadvice read-from-minibuffer (before skk-viper-ad activate)
      "minibuffer-setup-hook に skk-cursor-setup-minibuffer をフックする。
viper-read-string-with-history は minibuffer-setup-hook を関数ローカル
にしてしまうので、予め minibuffer-setup-hook にかけておいたフックが無効
となる。"
      (add-hook 'minibuffer-setup-hook 'skk-cursor-setup-minibuffer 'append)))

(skk-viper-advice-select
 viper-forward-word-kernel vip-forward-word-kernel
 (around skk-ad activate)
 ("SKK モードがオンで、ポイントの直後の文字が JISX0208 だったら forward-word する。"
  (if (and skk-mode (skk-jisx0208-p (following-char)))
      (forward-word val)
    ad-do-it )))

(skk-viper-advice-select
 viper-backward-word-kernel vip-backward-word-kernel
 (around skk-ad activate)
 ("SKK モードがオンで、ポイントの直前の文字が JISX0208 だったら backward-word する。"
  (if (and skk-mode (skk-jisx0208-p (preceding-char)))
      (backward-word val)
    ad-do-it )))

;; please sync with advice to delete-backward-char
(skk-viper-advice-select
 viper-del-backward-char-in-insert vip-del-backward-char-in-insert
 (around skk-ad activate)
 ("▼モードで skk-delete-implies-kakutei が non-nil だったら直前の文字を消して確定する。
▼モードで skk-delete-implies-kakutei が nil だったら前候補を表示する。
▽モードだったら確定する。
確定入力モードで、かなプレフィックスの入力中ならば、かなプレフィックスを消す。"
  (let ((count (or (prefix-numeric-value (ad-get-arg 0)) 1)))
    (cond (skk-henkan-active
	   (if (and (not skk-delete-implies-kakutei)
		    (= skk-henkan-end-point (point)))
	       (skk-previous-candidate)
	     ;;(if skk-use-face (skk-henkan-face-off))
 	     ;; overwrite-mode で、ポイントが全角文字に囲まれていると
	     ;; きに delete-backward-char を使うと、全角文字は消すが半
	     ;; 角文字分しか backward 方向にポイントが戻らない (Emacs
	     ;; 19.31 にて確認)。変換中の候補に対しては
	     ;; delete-backward-char で必ず全角文字 1 文字分 backward
	     ;; 方向に戻った方が良い。
	     (if overwrite-mode
		 (progn
		   (backward-char count)
		   (delete-char count))
	       ad-do-it )
	     ;; XXX assume skk-prefix has no multibyte chars.
	     (if (> (length skk-prefix) count)
		 (setq skk-prefix (substring skk-prefix 0 (- (length skk-prefix) count)))
	       (setq skk-prefix ""))
	     (if (>= skk-henkan-end-point (point)) (skk-kakutei))))
	  ((and skk-henkan-on (>= skk-henkan-start-point (point)))
	   (setq skk-henkan-count 0)
	   (skk-kakutei))
	  ;; 入力中の見出し語に対しては delete-backward-char で必ず全角文字 1
	  ;; 文字分 backward 方向に戻った方が良い。
	  ((and skk-henkan-on overwrite-mode)
	   (backward-char count)
	   (delete-char count))
	  (t
	   (if (string= skk-prefix "")
	       ad-do-it
	     (skk-erase-prefix 'clean)))))))

(skk-viper-advice-select
 viper-intercept-ESC-key vip-intercept-ESC-key
 (before skk-add activate)
 ("▽モード、▼モードだったら確定する。"
  (and skk-mode skk-henkan-on (skk-kakutei))))

(skk-viper-advice-select
 viper-join-lines vip-join-lines
 (after skk-ad activate)
 ("スペースの両側の文字セットが JISX0208 だったらスペースを取り除く。" ;
  (save-match-data
    (and (skk-jisx0208-p
	  (char-after (progn (skip-chars-forward " ") (point))))
	 (skk-jisx0208-p
	  (char-before (progn (skip-chars-backward " ") (point))))
	 (while (looking-at " ")
	   (delete-char 1))))))

;;; Functions.
;;;###autoload
(defun skk-viper-normalize-map ()
  (let ((other-buffer
	 (static-if (eq skk-emacs-type 'xemacs)
	     (local-variable-p 'minor-mode-map-alist nil t)
	   (local-variable-if-set-p 'minor-mode-map-alist))))
    ;; for current buffer and buffers to be created in the future.
    ;; substantially the same job as viper-harness-minor-mode does.
    (funcall skk-viper-normalize-map-function)
    (setq-default minor-mode-map-alist minor-mode-map-alist)
    (if (not other-buffer)
	nil
      ;; for buffers which are already created and have the minor-mode-map-alist
      ;; localized by Viper.
      (save-current-buffer
	(let ((buf (buffer-list)))
	  (while buf
	    (set-buffer (car buf))
	    (if (null (assq 'skk-j-mode minor-mode-map-alist))
		(progn
		  (set-modified-alist
		   'minor-mode-map-alist
		   (list (cons 'skk-latin-mode skk-latin-mode-map)
			 (cons 'skk-abbrev-mode skk-abbrev-mode-map)
			 (cons 'skk-j-mode skk-j-mode-map)
			 (cons 'skk-jisx0208-latin-mode
			       skk-jisx0208-latin-mode-map)))))
	    (funcall skk-viper-normalize-map-function)
	    (setq buf (cdr buf))))))))

(eval-after-load "viper-cmd"
  '(defun viper-toggle-case (arg)
     "Toggle character case.
Convert hirakana to katakana and vice versa."
     (interactive "P")
     (let ((val (viper-p-val arg)) (c))
       (viper-set-destructive-command
	(list 'viper-toggle-case val nil nil nil nil))
       (while (> val 0)
	 (setq c (following-char))
	 (delete-char 1 nil)
	 (cond ((skk-ascii-char-p c)
		(if (eq c (upcase c))
		    (insert-char (downcase c) 1)
		  (insert-char (upcase c) 1)))
	       ((and (<= ?ぁ c) (>= ?ん c))
		(insert-string
		 (skk-hiragana-to-katakana (char-to-string c))))
	       ((and (<= ?ァ c) (>= ?ン c))
		(insert-string
		 (skk-katakana-to-hiragana (char-to-string c))))
	       (t (insert-char c 1)))
	 (if (eolp) (backward-char 1))
	 (setq val (1- val))))))

;; viper-toggle-key-action と連動させる？
(skk-viper-normalize-map)
(add-hook 'skk-mode-hook 'skk-mode-once-again)

(provide 'skk-viper)
;;; skk-viper.el ends here
