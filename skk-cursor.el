;;; skk-cursor.el --- SKK cursor control.
;; Copyright (C) 1996, 1997, 1998, 1999
;; Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>

;; Author: Masatake YAMATO <jet@airlab.cs.ritsumei.ac.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-cursor.el,v 1.1.2.5.2.18 1999/12/14 12:43:10 furue Exp $
;; Keywords: japanese
;; Last Modified: $Date: 1999/12/14 12:43:10 $

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
(eval-when-compile (require 'static)
		   (require 'skk-macs) (require 'skk-vars) )

;; functions.
(defun skk-cursor-set-color (color)
  ;; カーソルの色を COLOR に変更する。
  (and skk-use-color-cursor
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
	(modify-frame-parameters (selected-frame) '((cursor-type . box))) ))))

(defun skk-cursor-current-color ()
  ;; カレントバッファの SKK のモードから、カーソルの色を取得する。
  (if (not skk-mode)
      skk-cursor-default-color
    (cond (skk-jisx0208-latin-mode
	   skk-cursor-jisx0208-latin-color )
	  (skk-abbrev-mode skk-cursor-abbrev-color)
	  (skk-katakana skk-cursor-katakana-color)
	  (skk-j-mode skk-cursor-hiragana-color)
	  ((and (boundp 'skk-jisx0201-mode)
		skk-jisx0201-mode)
	   skk-cursor-jisx0201-color)
	  (t skk-cursor-latin-color) )))

;; Overwrite by skk-viper.el
(defun skk-cursor-set-properly ()
  ;; カレントバッファの SKK のモードに従い、カーソルの色を変更する。
  (if (not (get-buffer-window (current-buffer)))
      nil
    (and skk-use-color-cursor 
	 (skk-cursor-set-color (skk-cursor-current-color)) )
    (and skk-cursor-change-width (skk-cursor-change-when-ovwrt)) ))

;;; advices.
;; cover to original Emacs functions.
(defadvice overwrite-mode (after skk-cursor-ad activate)
  "skk-cursor-change-width が non-nil だったら、カーソルの幅を縮める。"
  (and skk-cursor-change-width (skk-cursor-change-when-ovwrt)) )

;;;;;(defadvice abort-recursive-edit (before skk-cursor-ad activate)
;;;;;  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;;;;  (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly)) )
;;;;;
;;;;;(defadvice exit-minibuffer (before skk-cursor-ad activate)
;;;;;  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;;;;  (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly))
;;;;;  (skk-cursor-set-properly) )

;; 別のバッファへ飛ぶコマンドは skk-mode が nil でもカーソル色を調整する必要が
;; ある。
(let ((funcs '(kill-buffer
	       bury-buffer
	       switch-to-buffer
	       pop-to-buffer
	       other-window
	       delete-window
	       select-window
	       select-frame
	       delete-frame)))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after skk-cursor-ad activate)
	"入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
	(skk-cursor-set-properly) ) ) )
    (setq funcs (cdr funcs)) ) )

;;(defadvice goto-line (after skk-cursor-ad activate)
;;  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;  (and skk-mode (skk-cursor-set-properly)) )

(defadvice yank (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice yank-pop (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice recenter (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

;;(defadvice insert-file (after skk-cursor-ad activate)
;;  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;  (and skk-mode (skk-cursor-set-properly)) )

(defadvice newline (around skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if skk-abbrev-mode
      (progn ad-do-it (skk-cursor-set-properly))
    ad-do-it ))

;; cover to hilit19 functions.
(defadvice hilit-yank (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice hilit-yank-pop (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice hilit-recenter (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode (skk-cursor-set-properly)) )

;;;;;(defadvice execute-extended-command (around skk-cursor-ad activate preactivate)
;;;;;  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;;;;  (if skk-mode
;;;;;		(unwind-protect ad-do-it (skk-cursor-set-properly))
;;;;;	      ad-do-it ))
;;;;;
;;;;;(static-unless (eq skk-emacs-type 'xemacs)
;;;;;  (defadvice completing-read (around skk-cursor-ad activate preactivate)
;;;;;	 "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;;;;	 (if skk-mode
;;;;;	     (condition-case err
;;;;;		 (progn ad-do-it (skk-cursor-set-properly))
;;;;;	       (error
;;;;;		(skk-cursor-set-properly)
;;;;;		(signal (car err) (cdr err)) )
;;;;;	       (quit
;;;;;		(skk-cursor-set-properly)
;;;;;		(signal 'quit nil) ))
;;;;;	   ad-do-it ))
;;;;;  (defadvice read-from-minibuffer (around skk-cursor-ad activate preactivate)
;;;;;	 "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;;;;	 (if skk-mode
;;;;;	     (condition-case err
;;;;;		 (progn ad-do-it (skk-cursor-set-properly))
;;;;;	       (error
;;;;;		(skk-cursor-set-properly)
;;;;;		(signal (car err) (cdr err)) )
;;;;;	       (quit
;;;;;		(skk-cursor-set-properly)
;;;;;		(signal 'quit nil) ))
;;;;;	   ad-do-it )) )
;;;;;
;;;;;(if (eq skk-emacs-type 'xemacs)
;;;;;	 (defadvice minibuffer-keyboard-quit (before skk-cursor-ad activate)
;;;;;	   "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
;;;;;	   (with-current-buffer (skk-minibuffer-origin) (skk-cursor-set-properly))
;;;;;	   (skk-cursor-set-properly) ))

(defadvice keyboard-quit (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (and skk-mode
       (not skk-henkan-on)
       (not skk-henkan-active)
       (skk-cursor-set-properly) ) )

;; cover to VIP/Viper functions.
(defadvice viper-intercept-ESC-key (after skk-cursor-ad activate)
  (and skk-mode (skk-cursor-set-properly)) )

(defadvice vip-intercept-ESC-key (after skk-cursor-ad activate)
  (and skk-mode (skk-cursor-set-properly)) )

;; cover to SKK functions.
(defadvice skk-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-latin-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-jisx0208-latin-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-abbrev-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-mode (after skk-cursor-ad activate)
  (skk-cursor-set-properly) )

(defadvice skk-auto-fill-mode (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-color (if skk-mode
			    skk-cursor-hiragana-color
			  skk-cursor-default-color )))

(defadvice skk-toggle-kana (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if skk-katakana
      (skk-cursor-set-color skk-cursor-katakana-color)
    (skk-cursor-set-color skk-cursor-hiragana-color) ))

(defadvice skk-kakutei (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if (interactive-p)
      (skk-cursor-set-color (if skk-katakana skk-cursor-katakana-color
			      skk-cursor-hiragana-color ))))

(defadvice skk-save-jisyo (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-katakana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-hiragana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-jisx0208-latin-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-latin-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-jisx0201-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-message (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-katakana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-gyakubiki-katakana-message (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-message (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-hurigana-katakana-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-romaji-region (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-romaji-message (after skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (skk-cursor-set-properly) )

(defadvice skk-delete-backward-char (around skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if skk-abbrev-mode
      (progn ad-do-it (skk-cursor-set-properly))
    ad-do-it ))

(defadvice skk-start-henkan (around skk-cursor-ad activate)
  "入力モードに応じカーソル色を変化させる。Ovwrt モードのときにカーソル幅を小さくする。"
  (if skk-abbrev-mode
      (progn ad-do-it (skk-cursor-set-properly))
    ad-do-it ))

;;(add-hook 'after-make-frame-hook 'skk-cursor-set-properly)

(add-hook 'minibuffer-setup-hook
	  (function
	   (lambda ()
	     (setq skk-cursor-color-before-entering-minibuffer
		   (with-current-buffer
		       (skk-minibuffer-origin) (skk-cursor-current-color)))
	     (skk-cursor-set-properly) ) ) )

(add-hook 'minibuffer-exit-hook
	  (function
	   (lambda ()
	     (skk-cursor-set-color
	      skk-cursor-color-before-entering-minibuffer)
	     (and skk-cursor-change-width (skk-cursor-change-when-ovwrt)) ))
	  'append )

;; 最初に load されたときは、skk-cursor adviced function になる前の関数によって
;; 呼ばれており、advice が効いてないので、トップレベルでカーソルを合わせておく。
;; ここじゃ効かないのか...(;_;)
;;(and (interactive-p) (skk-cursor-set-properly))

(provide 'skk-cursor)
;;; Local Variables:
;;; End:
;;; skk-cursor.el ends here
