;;; skk-cursor.el --- SKK cursor control.
;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;; Masatake YAMATO <masata-y@is.aist-nara.ac.jp> 

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-cursor.el,v 1.1.2.5.2.27 2000/08/16 12:46:03 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/08/16 12:46:03 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; Code:
(or (skk-color-display-p) (error "SKK-CURSOR requires color display"))
(eval-when-compile (require 'static)
		   (require 'skk-macs) (require 'skk-vars))

;; functions.
(defun skk-cursor-set-color (color)
  ;; カーソルの色を COLOR に変更する。
  (if (not color)
      nil
    (condition-case nil
	(set-cursor-color color)
      (error
       (set-cursor-color skk-cursor-default-color)
       (and skk-cursor-report-set-error
	    (skk-message
	     "カラーマップ切れです。ディフォルトのカラーを使います。"
	     "Color map is exhausting, use default cursor color" ))))))

(defun skk-cursor-change-when-ovwrt ()
  (static-cond
   ((eq skk-emacs-type 'xemacs) (setq bar-cursor overwrite-mode))
   (t (if overwrite-mode
	  (modify-frame-parameters (selected-frame) '((cursor-type bar . 3)))
	(modify-frame-parameters (selected-frame) '((cursor-type . box)))))))

(defun skk-cursor-current-color ()
  ;; カレントバッファの SKK のモードから、カーソルの色を取得する。
  (cond ((not skk-mode) skk-cursor-default-color)
	;; skk-start-henkan の中では、skk-j-mode フラグを立てながら、
	;; skk-abbrev-mode フラグも立てている (変換後、直後に入力する文字が
	;; 元の入力モードにて行なわれるように)。従い、skk-abbrev-mode フラ
	;; グのチェックの優先度を上げる。
	(skk-abbrev-mode skk-cursor-abbrev-color)
	(skk-jisx0208-latin-mode
	 skk-cursor-jisx0208-latin-color )
	(skk-katakana skk-cursor-katakana-color)
	(skk-j-mode skk-cursor-hiragana-color)
	(skk-jisx0201-mode skk-cursor-jisx0201-color)
	(t skk-cursor-latin-color)))

(defun skk-cursor-set-properly (&optional color)
  ;; カレントバッファの SKK のモードに従い、カーソルの色を変更する。
  ;; オプショナル引数の COLOR が指定されたときは、そのカーソル色を使う。
  ;; OVWRT モードのときはカーソルの幅を小さくする。
   (if (not (get-buffer-window (current-buffer)))
      nil
    (and skk-use-color-cursor
	 (skk-cursor-set-color (cond (color)
				     (t (skk-cursor-current-color)))))
    (and skk-cursor-change-width (skk-cursor-change-when-ovwrt))))

(defun skk-cursor-setup-minibuffer ()
  (setq skk-cursor-color-before-entering-minibuffer
	(with-current-buffer
	    (skk-minibuffer-origin) (skk-cursor-current-color)))
  (skk-cursor-set-properly)
  (static-when (eq skk-emacs-type 'xemacs)
    (cond ((and (memq this-command '(skk-start-henkan skk-insert))
		(with-current-buffer
		    (skk-minibuffer-origin) skk-abbrev-mode))
	   (skk-cursor-set-color skk-cursor-hiragana-color))
	  ((and (memq this-command '(skk-insert))
		(not (memq last-command-char
			   '(?\\))))
	   nil)
	  (t
	   (skk-cursor-set-color skk-cursor-default-color)))))

;;; advices.
;;入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。
;; 別のバッファへ飛ぶコマンドは skk-mode が nil でもカーソル色を調整する必要がある。
;; CLASS は after.
(let ((funcs '(
	       ;; cover to original Emacs functions.
	       bury-buffer
	       delete-frame
	       delete-window
	       ;;execute-extended-command
	       kill-buffer
	       other-window
	       overwrite-mode
	       pop-to-buffer
	       select-frame
	       select-window
	       switch-to-buffer
	       ;; cover to SKK functions.
	       skk-auto-fill-mode
	       skk-gyakubiki-katakana-message
	       skk-gyakubiki-katakana-region
	       skk-gyakubiki-message
	       skk-hiragana-region
	       skk-hurigana-katakana-region
	       skk-hurigana-message
	       skk-hurigana-region
	       skk-jisx0201-region
	       skk-jisx0208-latin-region
	       skk-katakana-region
	       skk-latin-region
	       skk-mode
	       skk-romaji-message
	       skk-romaji-region
	       skk-save-jisyo
	       skk-toggle-kana)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor-ad activate)
	"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
	(skk-cursor-set-properly))))
    (setq funcs (cdr funcs))))

;;入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。
;; CLASS は after.
;; skk-mode が nil か non-nil かの判定付き。
(let ((funcs '(goto-line
	       insert-file
	       keyboard-quit
	       recenter
	       yank
	       yank-pop
	       ;; cover to hilit functions.
	       hilit-recenter
	       hilit-yank
	       hilit-yank-pop)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor-ad activate)
	"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
	(and skk-mode (skk-cursor-set-properly)))))
    (setq funcs (cdr funcs))))

;;入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。
;; CLASS は before.
;; ミニバッファから元のカレントバッファを探し出し、カーソルをセット。
(let ((funcs '(exit-minibuffer)))
  (static-if (eq skk-emacs-type 'xemacs)
      (setq funcs (cons 'minibuffer-keyboard-quit funcs)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(before skk-cursor-ad activate)
	"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
	(with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly)))))
    (setq funcs (cdr funcs))))

;; 入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。
;; CLASS は around.
;; skk-abbrev-mode のときだけカーソルをセット。
(let ((funcs '(
	       ;; cover to original Emacs functions.
	       newline
	       ;; cover to SKK functions.
	       skk-delete-backward-char
	       skk-insert
	       skk-start-henkan
	       )))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(around skk-cursor-ad activate preactivate)
	"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
	;; CLASS は around.
	;; skk-abbrev-mode のときだけカーソルをセット。
	(if skk-abbrev-mode
	    (progn ad-do-it (skk-cursor-set-properly))
	  ad-do-it))))
    (setq funcs (cdr funcs))))

;;(defadvice execute-extended-command (around skk-cursor-ad activate preactivate)
;;  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;  (if skk-mode
;;		(unwind-protect ad-do-it (skk-cursor-set-properly))
;;	      ad-do-it ))
;;
;;(static-unless (eq skk-emacs-type 'xemacs)
;;  (defadvice completing-read (around skk-cursor-ad activate preactivate)
;;	 "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;	 (if skk-mode
;;	     (condition-case err
;;		 (progn ad-do-it (skk-cursor-set-properly))
;;	       (error
;;		(skk-cursor-set-properly)
;;		(signal (car err) (cdr err)))
;;	       (quit
;;		(skk-cursor-set-properly)
;;		(signal 'quit nil)))
;;	   ad-do-it ))
;;  (defadvice read-from-minibuffer (around skk-cursor-ad activate preactivate)
;;	 "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;	 (if skk-mode
;;	     (condition-case err
;;		 (progn ad-do-it (skk-cursor-set-properly))
;;	       (error
;;		(skk-cursor-set-properly)
;;		(signal (car err) (cdr err)))
;;	       (quit
;;		(skk-cursor-set-properly)
;;		(signal 'quit nil)))
;;	   ad-do-it )))
;;
;;(defadvice insert-file (after skk-cursor-ad activate)
;;  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;  (and skk-mode (skk-cursor-set-properly)) )

(static-when (featurep 'xemacs)
  (defadvice abort-recursive-edit (before skk-cursor-ad activate preactivate)
    "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
    (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly))))

;; cover to SKK functions.
(defadvice skk-latin-mode (after skk-cursor-ad activate)
  "カーソル色を skk-cursor-latin-color に変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly skk-cursor-latin-color))

(defadvice skk-jisx0208-latin-mode (after skk-cursor-ad activate)
  "カーソル色を skk-cursor-jisx0208-latin-color に変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly skk-cursor-jisx0208-latin-color))

(defadvice skk-abbrev-mode (after skk-cursor-ad activate)
  "応じカーソル色を skk-cursor-abbrev-color に変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly skk-cursor-abbrev-color))

(defadvice skk-kakutei (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if (interactive-p) (skk-cursor-set-properly)))

(add-hook 'minibuffer-setup-hook 'skk-cursor-setup-minibuffer)
(add-hook 'minibuffer-exit-hook
	  (function
	   (lambda ()
	     (with-current-buffer (skk-minibuffer-origin)
	       (skk-cursor-set-color
		skk-cursor-color-before-entering-minibuffer)
	       (and skk-cursor-change-width (skk-cursor-change-when-ovwrt)))))
	  'append )

;;; Hooks
(add-hook 'isearch-mode-end-hook 'skk-cursor-set-properly 'append)

(defalias 'skk-set-cursor-color 'skk-cursor-set-color)
(defalias 'skk-change-cursor-when-ovwrt 'skk-cursor-change-when-ovwrt)
(defalias 'skk-set-cursor-properly 'skk-cursor-set-properly)

(add-hook 'skk-mode-hook 'skk-mode-once-again)

(provide 'skk-cursor)
;;; Local Variables:
;;; End:
;;; skk-cursor.el ends here
