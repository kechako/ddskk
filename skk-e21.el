;;; skk-e21.el --- GNU Emacs 21 support for SKK

;; Copyright (C) 1999-2005 SKK Development Team <skk@ring.gr.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'static)
  (require 'tooltip))

(eval-and-compile
  (autoload 'Info-goto-node "info")
  (autoload 'browse-url "browse-url"))

;; Variables.
(defvar skk-e21-modeline-menu-items
  (when window-system
    '("Daredevil SKK Menu"
      ["Hiragana"
       (skk-j-mode-on)
       :selected (and skk-j-mode (not skk-katakana))
       :style radio
       :keys nil
       :key-sequence nil]
      ["Katakana"
       (skk-j-mode-on t)
       :selected (and skk-j-mode skk-katakana)
       :style radio
       :keys nil
       :key-sequence nil]
      ["Hankaku alphabet"
       skk-latin-mode
       :selected skk-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      ["Zenkaku alphabet"
       skk-jisx0208-latin-mode
       :selected skk-jisx0208-latin-mode
       :style radio
       :keys nil
       :key-sequence nil]
      "--"
      ["Read Manual" skk-e21-info t]
      ["Start Tutorial" skk-tutorial t]
      ["Customize Daredevil SKK" skk-customize t]
      ["Send a Bug Report"
       (let (skk-japanese-message-and-error)
	 (skk-submit-bug-report)) t]
      "--"
      ["About Daredevil SKK..." skk-version t]
      ["Visit Daredevil Web Site" skk-e21-visit-openlab t])))

(defvar skk-e21-menu-resource-ja
  '(("Daredevil SKK Menu" . "Daredevil SKK $B%a%K%e!<(B")
    ("Convert Region and Echo" . "$BNN0h$rJQ49$7$F%_%K%P%C%U%!$KI=<((B")
    ("Gyakubiki" . "$B5U0z$-(B")
    ("to Hiragana" . "$B$R$i$,$J$KJQ49(B")
    ("to Hiragana, All Candidates" . "$B$R$i$,$J$KJQ49!"A4$F$N8uJd$rI=<((B")
    ("to Katakana" . "$B%+%?%+%J$KJQ49(B")
    ("to Katakana, All Candidates" . "$B%+%?%+%J$KJQ49!"A4$F$N8uJd$rI=<((B")
    ("Hurigana" . "$B$U$j$,$J(B")
    ("Convert Region and Replace" . "$BNN0h$rJQ49$7$FCV$-49$($k(B")
    ("Ascii" . "$BA43Q1Q?t$r(B ASCII $B$KJQ49(B")
    ("Hiragana" . "$B$R$i$,$J(B")
    ("Katakana" . "$B%+%?%+%J(B")
    ("Romaji" . "$B%m!<%^;z$KJQ49(B")
    ("Zenkaku" . "ASCII $B$rA43Q1Q?t$KJQ49(B")
    ("Count Jisyo Candidates" . "$B<-=qCf$N8uJd?t$r?t$($k(B")
    ("Save Jisyo" . "$B<-=q$rJ]B8$9$k(B")
    ("Undo Kakutei" . "$B3NDj$r<h$j>C$9(B ($B%"%s%I%%!<(B)")
    ("Version" . "SKK $B$N%P!<%8%g%s(B")
    ("Daredevil SKK Menu" . "Daredevil SKK $B%a%K%e!<(B")
    ("Hankaku alphabet" . "$BH>3Q1Q?t(B")
    ("Zenkaku alphabet" . "$BA43Q1Q?t(B")
    ("Read Manual" . "$B%^%K%e%"%k$rFI$`(B")
    ("Start Tutorial" . "$B%A%e!<%H%j%"%k(B")
    ("Customize Daredevil SKK" . "Daredevil SKK $B$r%+%9%?%^%$%:(B")
    ("Send a Bug Report" . "$B%P%0$rJs9p$9$k(B")
    ("About Daredevil SKK..." . "Daredevil SKK $B$K$D$$$F(B...")
    ("Visit Daredevil Web Site" . "Daredevil SKK $B$N%5%$%H$X(B")))

(defvar skk-e21-modeline-property
  (when window-system
    (list 'local-map (let ((map (make-sparse-keymap)))
		       (define-key map [mode-line mouse-3]
			 #'skk-e21-modeline-menu)
		       (define-key map [mode-line mouse-1]
			 #'skk-e21-circulate-modes)
		       map)
	  'help-echo
	  "mouse-1: $B%b!<%I@ZBX(B($B=[4D(B), mouse-3: SKK $B%a%K%e!<(B"
	  'mouse-face
	  'highlight)))

(defvar skk-e21-property-alist
  (when window-system
    (list
     (cons 'latin skk-e21-modeline-property))))

(defvar skk-e21-coding-system (if (memq window-system '(w32 nil))
				  nil
				locale-coding-system))

;; Functions.

(defun skk-e21-modeline-menu ()
  (interactive)
  ;; Find keys
  (aset (nth 1 skk-e21-modeline-menu-items)
	7
	(cond (skk-katakana
	       (skk-e21-find-func-keys 'skk-toggle-kana))
	      ((not skk-mode)
	       (skk-e21-find-func-keys 'skk-mode))
	      ((not skk-j-mode)
	       (skk-e21-find-func-keys 'skk-kakutei))
	      (t
	       nil)))
  (aset (nth 2 skk-e21-modeline-menu-items)
	7
	(if (and skk-j-mode
		 (not skk-katakana))
	    (skk-e21-find-func-keys 'skk-toggle-kana)
	  nil))
  (aset (nth 3 skk-e21-modeline-menu-items)
	7
	(if skk-j-mode
	    (skk-e21-find-func-keys 'skk-latin-mode)
	  nil))
  (aset (nth 4 skk-e21-modeline-menu-items)
	7
	(if skk-j-mode
	    (skk-e21-find-func-keys 'skk-jisx0208-latin-mode)
	  nil))
  ;;
  (let ((easy-menu-converted-items-table
	 (make-hash-table :test 'equal)))
    (popup-menu skk-e21-modeline-menu-items)))

(defun skk-e21-circulate-modes (&optional arg)
  (interactive "P")
  (cond
   (skk-henkan-mode
    nil)
   ((not skk-mode)
    (skk-mode arg))
   (skk-j-mode
    (if skk-katakana
	(skk-jisx0208-latin-mode arg)
      (skk-toggle-kana arg)))
   (skk-jisx0208-latin-mode
    (skk-latin-mode arg))
   (skk-latin-mode
    (skk-j-mode-on))))

(defun skk-e21-info ()
  (interactive)
  (Info-goto-node "(skk)"))

(defun skk-e21-customize ()
  (interactive)
  (customize-group "skk"))

(defun skk-e21-visit-openlab ()
  (interactive)
  (browse-url "http://openlab.ring.gr.jp/skk/index-j.html"))

;;;###autoload
(defun skk-e21-prepare-modeline-properties ()
  (setq skk-icon
	(let* ((dir (file-name-directory skk-tut-file))
	       (image (find-image
		       `((:type xpm
				:file ,(expand-file-name "skk.xpm" dir)
				:ascent center))))
	       (string "dummy"))
	  (if (and skk-show-icon window-system image)
	      (apply 'propertize string
		     (cons 'display (cons image skk-e21-modeline-property)))
	    nil)))
  ;;
  (unless skk-use-color-cursor
    (setq skk-indicator-use-cursor-color nil))
  ;;
  (when window-system
    (let (face)
      (dolist (mode '(hiragana
		      katakana
		      jisx0208-latin
		      jisx0201
		      abbrev))
	(setq face (intern (format "skk-e21-%s-face" mode)))
	(unless (facep face)
	  (make-face face)
	  (when skk-indicator-use-cursor-color
	    (set-face-foreground face
				 (symbol-value
				  (intern
				   (format "skk-cursor-%s-color"
					   mode))))))
	(push (cons mode (append skk-e21-modeline-property
				 (list 'face face)))
	       skk-e21-property-alist)))))

(defun skk-e21-find-func-keys (func)
  (let ((keys
	 (or (do ((spec (nth 4 skk-rule-tree) (cdr spec))
		  (list nil (car spec))
		  (str nil (when (eq (nth 3 list)
				     func)
			     (nth 1 list))))
		 ((or str (null spec))
		  (if (stringp str)
		      str
		    nil)))
	     (car (where-is-internal func skk-j-mode-map)))))
    (if keys
	(format "%s" (key-description keys))
      nil)))

(defun skk-e21-encode-string (str)
  (if (null skk-e21-coding-system)
      str
    (encode-coding-string str skk-e21-coding-system)))

(defun skk-e21-menu-replace (list)
  (let (cons)
    (while list
      (cond
       ((listp (car list))
	(skk-e21-menu-replace (car list)))
       ((and (stringp (car list))
	     (setq cons (assoc (car list) skk-e21-menu-resource-ja)))
	(setcar list (skk-e21-encode-string (cdr cons))))
       ((and (vectorp (car list))
	     (setq cons (assoc (aref (car list) 0) skk-e21-menu-resource-ja)))
	(aset (car list) 0 (skk-e21-encode-string (cdr cons)))))
      (setq list (cdr list)))))

(defun skk-e21-mouse-position ()
  "Return the position of point as (FRAME X . Y).
Analogous to mouse-position."
  (let* ((w (if skk-isearch-switch
		(minibuffer-window)
	      (selected-window)))
	 (edges (window-edges w))
	 (list
	  (compute-motion (max (window-start w) (point-min))   ; start pos
			  ;; window-start can be < point-min if the
			  ;; latter has changed since the last redisplay
			  '(0 . 0)       ; start XY
			  (or (ignore-errors
				(marker-position
				 skk-henkan-start-point))
			      (point))       ; stop pos
			  (cons (window-width w)
				(window-height w)); stop XY: none
			  (1- (window-width w))       ; width
			  (cons (window-hscroll w) 0)     ; 0 may not be right?
			  w)))
    ;; compute-motion returns (pos HPOS VPOS prevhpos contin)
    ;; we want:               (frame hpos . vpos)
    (cons (selected-frame)
	  (cons (+ (car edges)       (car (cdr list)))
		(+ (car (cdr edges)) (car (cdr (cdr list))))))))

(defun skk-tooltip-show-at-point (text &optional listing)
  (require 'tooltip)
  (let* ((P (skk-e21-mouse-position))
	 (frame (car P))
	 (x (cadr P))
	 (y (cddr P))
	 (oP (mouse-position))
	 (oframe (car oP))
	 (ox (cadr oP))
	 (oy (cddr oP))
	 (inhibit-quit t)
	 event)
    (set-mouse-position frame x y)
    (skk-tooltip-show-1 text skk-tooltip-parameters)
    (setq event (next-command-event))
    (cond
     ((skk-key-binding-member (skk-event-key event)
			      '(keyboard-quit
				skk-kanagaki-bs
				skk-kanagaki-esc)
			      skk-j-mode-map)
      (tooltip-hide)
      (when (and ox oy)
	(set-mouse-position oframe ox oy))
      (skk-set-henkan-count 0)
      (skk-unread-event
       (character-to-event
	(aref (car (where-is-internal
		    'skk-previous-candidate
		    skk-j-mode-map))
	      0)))
      (when listing
	 ;; skk-henkan $B$^$G0l5$$K(B throw $B$9$k!#(B
	(throw 'unread nil)))
     (t
      (when (and ox oy)
	(set-mouse-position oframe ox oy))
      (tooltip-hide)
      (skk-unread-event event)))))

(defun skk-tooltip-show-1 (text skk-params)
  (condition-case error
      (let ((params (copy-sequence tooltip-frame-parameters))
	    fg bg)
	(if skk-params
	    ;; $B%f!<%6$,FH<+$K(B tooltip $BI=<(@_Dj$9$k(B
	    (dolist (cell skk-params)
	      (setq params (tooltip-set-param params
					      (car cell)
					      (cdr cell))))
	  ;; tooltip $B$N%G%U%)%k%H$N@_Dj$r$9$k(B
	  (setq fg (face-attribute 'tooltip :foreground))
	  (setq bg (face-attribute 'tooltip :background))
	  (when (stringp fg)
	    (setq params (tooltip-set-param params 'foreground-color fg))
	    (setq params (tooltip-set-param params 'border-color fg)))
	  (when (stringp bg)
	    (setq params (tooltip-set-param params 'background-color bg))))
	(unless (ignore-errors
		  (or (get-text-property 0 'face text)
		      (get-text-property 2 'face text)))
	  (setq text (propertize text 'face 'tooltip)))
	(x-show-tip text
		    (selected-frame)
		    params
		    skk-tooltip-hide-delay
		    tooltip-x-offset
		    tooltip-y-offset))
    (error
     (message "Error while displaying tooltip: %s" error)
     (sit-for 1)
     (message "%s" text))))

;; advices.

(defadvice tooltip-hide (after ccc-ad activate)
  (update-buffer-local-frame-params))

(require 'product)
(product-provide
    (provide 'skk-e21)
  (require 'skk-version))

;;; skk-e21.el ends here
