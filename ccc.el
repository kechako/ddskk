;;; ccc.el --- cursor color control.
;; Copyright (C) 2000 Masatake YAMATO <masata-y@is.aist-nara.ac.jp> 

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: ccc.el,v 1.1.2.2 2000/09/27 13:42:04 minakaji Exp $
;; Keywords: cursor
;; Last Modified: $Date: 2000/09/27 13:42:04 $

;; This software is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either versions 2, or (at your option) any later
;; version.

;; This software is distributed in the hope that it will be useful but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;; Buffer local frame parameters
;; --- cursor, foreground, background
;; --- TODO: support other frame parameters
;;           should use uni prefix for functions and variables?

;;; Code:
(eval-when-compile (require 'static))
 
;;; user variables.

;;; functions.
;;;###autoload
(defun update-buffer-local-frame-params ()
  (update-buffer-local-cursor-color)
  (update-buffer-local-foreground-color)
  (update-buffer-local-background-color))

;;
;; buffer-local-cursor
;;
(defun buffer-local-cursor-color-default ()
  (static-if (eq (string-match "XEmacs" emacs-version) 'xemacs)
      (frame-property (selected-frame) 'cursor-color)
    (cdr (assq 'cursor-color (frame-parameters (selected-frame))))))

(defun set-buffer-local-cursor-color (color-name)
  (interactive "sColor: ")
  (if (and color-name (> (length color-name) 0))
      (setq buffer-local-cursor-color color-name)
    (setq buffer-local-cursor-color buffer-local-cursor-color-default))
  (update-buffer-local-cursor-color))

(defun update-buffer-local-cursor-color ()
  (if (and buffer-local-cursor-color
	   (stringp buffer-local-cursor-color)
	   (not (string= (buffer-local-cursor-color-default)
			 buffer-local-cursor-color)))
      (condition-case error
	  (set-cursor-color buffer-local-cursor-color)
	(error (setq buffer-local-cursor-color nil)))))

;;
;; buffer-local-foreground-color
;;
(defun buffer-local-foreground-color-default ()
  (static-if (eq (string-match "XEmacs" emacs-version) 'xemacs)
      (frame-property (selected-frame) 'foreground-color)
    (cdr (assq 'foreground-color (frame-parameters (selected-frame))))))

(defun set-buffer-local-foreground-color (color-name)
  (interactive "sColor: ")
  (if (and color-name (> (length color-name) 0))
      (setq buffer-local-foreground-color color-name)
    (setq buffer-local-foreground-color buffer-local-foreground-color-default))
  (update-buffer-local-foreground-color))

(defun update-buffer-local-foreground-color ()
  (if (and buffer-local-foreground-color
	   (stringp buffer-local-foreground-color)
	   (not (string= (buffer-local-foreground-color-default)
			 buffer-local-foreground-color)))
      (condition-case error
	  (set-foreground-color buffer-local-foreground-color)
	(error (setq buffer-local-foreground-color nil)))))

;;
;; buffer-local-background-color
;;
(defun buffer-local-background-color-default ()
  (static-if (eq (string-match "XEmacs" emacs-version) 'xemacs)
      (frame-property (selected-frame) 'background-color)
    (cdr (assq 'background-color (frame-parameters (selected-frame))))))

(defun set-buffer-local-background-color (color-name)
  (interactive "sColor: ")
  (if (and color-name (> (length color-name) 0))
      (setq buffer-local-background-color color-name)
    (setq buffer-local-background-color buffer-local-background-color-default))
  (update-buffer-local-background-color))

(defun update-buffer-local-background-color ()
  (if (and buffer-local-background-color
	   (stringp buffer-local-background-color)
	   (not (string= (buffer-local-background-color-default)
			 buffer-local-background-color)))
      (condition-case error
	  (set-background-color buffer-local-background-color)
	(error (setq buffer-local-background-color nil)))))

;;; advices.
(defvar buffer-local-frame-params-ad-targets
  '(
    ;; cover to original Emacs functions.
    bury-buffer
    delete-frame
    delete-window
    execute-extended-command 
    kill-buffer
    other-window
    overwrite-mode
    pop-to-buffer
    select-frame
    select-window
    switch-to-buffer
    ;;
    ;;goto-line 
    ;;insert-file 
    ;;recenter 
    ;;yank
    ;;yank-pop 
    ;; cover to hilit functions.
    ;;hilit-recenter 
    ;;hilit-yank 
    ;;hilit-yank-pop 
    ;; cover to VIP/Viper functions.
    ;;vip-Append
    ;;vip-Insert
    ;;vip-insert
    ;;vip-intercept-ESC-key 
    ;;vip-open-line
    ;;viper-Append
    ;;viper-Insert
    ;;viper-hide-replace-overlay 
    ;;viper-insert
    ;;viper-intercept-ESC-key
    ;;viper-open-line
    ))

(let ((funcs buffer-local-frame-params-ad-targets))
  (while funcs
    (eval
     (`
      (defadvice (, (intern (symbol-name (car funcs))))
	(after buffer-local-frame-params-ad activate)
	"Update frame frame parameters if `buffer-local-*-color' given."
	(update-buffer-local-frame-params)
	)))
    (setq funcs (cdr funcs))))

;;; internal variables.
(defvar buffer-local-cursor-color-default (buffer-local-cursor-color-default))
(defvar buffer-local-cursor-color (buffer-local-cursor-color-default))
(make-variable-buffer-local 'buffer-local-cursor-color)

(defvar buffer-local-foreground-color-default (buffer-local-foreground-color-default))
(defvar buffer-local-foreground-color (buffer-local-foreground-color-default))
(make-variable-buffer-local 'buffer-local-foreground-color)

(defvar buffer-local-background-color-default  (buffer-local-background-color-default))
(defvar buffer-local-background-color (buffer-local-background-color-default))
(make-variable-buffer-local 'buffer-local-background-color)

;;; Hooks
(add-hook 'isearch-mode-end-hook 'update-buffer-local-frame-params 'append)
(add-hook 'minibuffer-setup-hook 'update-buffer-local-frame-params 'append)
(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (with-current-buffer (nth 1 (buffer-list))
	      'update-buffer-local-frame-params 'append) 'append))

(provide 'ccc)
;;; Local Variables:
;;; End:
;;; ccc.el ends here
