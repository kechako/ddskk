;;; skk-leim.el --- SKK related code for LEIM
;; Copyright (C) 1997, 1999, 2000
;; Murata Shuuichirou <mrt@astec.co.jp>
;;
;; Author: Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;; Version: $Id: skk-leim.el,v 1.5.2.3.2.4 2000/08/15 11:02:46 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/08/15 11:02:46 $

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

;;; Code:
(eval-when-compile (require 'skk-macs) (require 'skk-vars))

;;;###autoload
(defun skk-activate (&optional name)
  (setq inactivate-current-input-method-function 'skk-inactivate)
  (or (and (boundp 'skk-mode-invoked) skk-mode-invoked)
      (defadvice skk-setup-modeline (around skk-leim-ad activate)
	"SKK が Emacs の Input Method として呼び出されたときは、この関数は何もせ
ず、インジケータは Emacs の機構によって制御される。"))
  (skk-mode 1))

;;;###autoload
(defun skk-auto-fill-activate (&optional name)
  (setq inactivate-current-input-method-function 'skk-auto-fill-inactivate)
  (or (and (boundp 'skk-mode-invoked) skk-mode-invoked)
      (defadvice skk-setup-modeline (around skk-leim-ad activate)
	"SKK が Emacs の Input Method として呼び出されたときは、この関数は何もせ
ず、インジケータは Emacs の機構によって制御される。"))
  (skk-auto-fill-mode 1))

;;;###autoload
(defun skk-inactivate ()
  (skk-mode -1))

;;;###autoload
(defun skk-auto-fill-inactivate ()
  (skk-auto-fill-mode -1))

(defun skk-leim-change-input-method-title (title)
  (setq current-input-method-title title)
  ;;(setcar (nthcdr 3 (assoc input-method input-method-alist)) title)
  ;;(force-mode-line-update t)
  )

(defadvice skk-abbrev-mode (after skk-leim-ad activate)
  (skk-leim-change-input-method-title (substring skk-abbrev-mode-string 1)))

(defadvice skk-auto-fill-mode (after skk-leim-ad activate)
  (skk-leim-change-input-method-title
   (substring
    (if skk-katakana skk-katakana-mode-string skk-hiragana-mode-string)
    1)))

(defadvice skk-jisx0201-mode (after skk-leim-ad activate)
  (skk-leim-change-input-method-title (substring skk-jisx0201-mode-string 1)))

(defadvice skk-jisx0208-latin-mode (after skk-leim-ad activate)
  (skk-leim-change-input-method-title
   (substring skk-jisx0208-latin-mode-string 1)))

(defadvice skk-kakutei (after skk-leim-ad activate)
  (skk-leim-change-input-method-title
   (substring
    (if skk-katakana skk-katakana-mode-string skk-hiragana-mode-string)
    1)))

(defadvice skk-latin-mode (after skk-leim-ad activate)
  (skk-leim-change-input-method-title (substring skk-latin-mode-string 1)))

(defadvice skk-mode (after skk-leim-ad activate)
  (skk-leim-change-input-method-title
   (substring
    (if skk-katakana skk-katakana-mode-string skk-hiragana-mode-string)
    1)))

(defadvice skk-toggle-kana (after skk-leim-ad activate)
  (skk-leim-change-input-method-title
   (substring
    (if skk-katakana skk-katakana-mode-string skk-hiragana-mode-string)
    1)))

;;;###autoload
(if (fboundp 'register-input-method)
    (progn
      (require 'advice)
      (defadvice activate-input-method (before skk-leim-ad activate)
	(remove-hook 'isearch-mode-hook
		     (lambda () (and (boundp 'skk-mode) skk-mode
				     (skk-isearch-mode-setup))))
	(remove-hook 'isearch-mode-end-hook
		     (lambda ()
		       (and (boundp 'skk-mode) skk-mode (skk-isearch-mode-cleanup)))))
      (register-input-method
       "japanese-skk" "Japanese"
       'skk-activate "かな"
       "Simple Kana to Kanji conversion program")
  
      (register-input-method
       "japanese-skk-auto-fill" "Japanese"
       'skk-auto-fill-activate "かな"
       "Simple Kana to Kanji conversion program with auto-fill")))

(provide 'skk-leim)
;;; skk-leim.el ends here
