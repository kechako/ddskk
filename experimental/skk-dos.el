;;; skk-dos.el --- MS-DOS related codes for skk.el

;; Copyright (C) 1999 Tsukamoto Tetsuo

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Keywords: japanese

;; This file is not part of Daredevil SKK yet.

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

;; $B$3$l$O(B Daredevil SKK $B$r(B DOS $BMQ(B Emacs $B$GF0$+$9$?$a$N(B work around $B$G$9!#(B
;; $B%U%!%$%kL>$N@)8B$r6/0z$K2r7h$9$k$3$H$rL\E*$H$7$F$$$^$9!#(B($B4D6-$K$h$C$F$O(B
;; long file name $B$,07$($k$h$&$G$9$,!#(B)
;; DOS $BMQ(B Emacs $B$O(B window-system $B<~$j$KFC<l$J<BAu$,$5$l$F$$$k$N$G$=$NJU$bB?>/(B
;; $B$4$^$+$7$^$9!#(B
;; $B%$%s%9%H!<%k$^$G$O%5%]!<%H$7$F$$$^$;$s!#(BEmacs Lisp $B$N%=!<%9$r9%$-$J=j$K(B
;; $B%3%T!<$7$F(B load-path $B$r@_Dj$7$F$/$@$5$$!#$=$N8e(B ~/_emacs $B$K(B
;;
;; (require 'skk-dos)
;;
;; $B$H=q$$$F$/$@$5$$!#$=$NB>$N@_Dj$O(B info $B$r;2>H$7$F$/$@$5$$!#(B

;;; Code:

;(require 'invisible "invisi~1")

;; Autoloads.
(autoload 'skk-abbrev-search "skk-ab~1" nil nil nil)
(autoload 'skk-ad-to-gengo "skk-ga~1" nil nil nil)
(autoload 'skk-adjust-search-prog-list-for-auto-okuri "skk-auto" nil nil nil)
(autoload 'skk-auto-fill-mode "skk" nil t nil)
(autoload 'skk-calc "skk-ga~1" nil nil nil)
(autoload 'skk-clock "skk-ga~1" nil t nil)
(autoload 'skk-compile-rule-list "skk" nil nil nil)
(autoload 'skk-completion "skk-comp" nil nil nil)
(autoload 'skk-current-date "skk-ga~1" nil nil nil)
(autoload 'skk-display-code-for-char-at-point "skk-kc~1" nil t nil)
(autoload 'skk-gengo-to-ad "skk-ga~1" nil nil nil)
(autoload 'skk-henkan-face-off-and-remove-itself "skk-ga~1" nil nil nil)
(autoload 'skk-ignore-dic-word "skk-ga~1" nil nil nil)
(autoload 'skk-input-by-code-or-menu "skk-kc~1" nil t nil)
(autoload 'skk-isearch-mode-cleanup "skk-is~1" nil nil nil)
(autoload 'skk-isearch-mode-setup "skk-is~1" nil nil nil)
(autoload 'skk-jisx0201-mode "skk-ji~1" nil t nil)
(autoload 'skk-minus "skk-ga~1" nil nil nil)
(autoload 'skk-mode "skk" nil t nil)
(autoload 'skk-num "skk-num" nil nil nil)
(autoload 'skk-num-compute-henkan-key "skk-num" nil nil nil)
(autoload 'skk-num-henkan-key "skk-num" nil nil nil)
(autoload 'skk-num-initialize "skk-num" nil nil nil)
(autoload 'skk-num-process-user-minibuf-input "skk-num" nil nil nil)
(autoload 'skk-num-uniq "skk-num" nil nil nil)
(autoload 'skk-num-update-jisyo "skk-num" nil nil nil)
(autoload 'skk-obsolete-check "skk-ob~1" nil t nil)
(autoload 'skk-obsolete-check-all-files "skk-ob~1" nil t nil)
(autoload 'skk-obsolete-put-obsolete-mark "skk-ob~1" nil nil nil)
(autoload 'skk-okuri-search "skk-auto" nil nil nil)
(autoload 'skk-plus "skk-gadget" nil nil nil)
(autoload 'skk-previous-completion "skk-comp" nil nil nil)
(autoload 'skk-start-henkan-with-completion "skk-comp" nil t nil)
(autoload 'skk-study-read "skk-st~1" nil t nil)
(autoload 'skk-study-save "skk-st~1" nil t nil)
(autoload 'skk-study-search "skk-st~1" nil nil nil)
(autoload 'skk-study-update "skk-st~1" nil nil nil)
(autoload 'skk-submit-bug-report "skk-de~1" nil t nil)
(autoload 'skk-times "skk-ga~1" nil nil nil)
(autoload 'skk-today "skk-ga~1" nil t nil)
(autoload 'skk-toggle-katakana "skk-ji~1" nil t nil)
(autoload 'skk-tutorial "skk-tut" nil t nil)
(autoload 'skk-version "skk" nil t nil)
(autoload 'skk-viper-normalize-map "skk-vi~1" nil t nil)

;; Advice.
(defadvice require (around skk-dos-ad activate preactivate)
  "Just a work around for SKK.
May not work with a more complicated program like Gnus."
  (condition-case err
      ad-do-it
    (error
     (let* ((file (or (ad-get-arg 1) (format "%s" (ad-get-arg 0))))
	    (i 1)
	    str)
       (cond ((> (length file) 8)
	      (setq str (substring file 0 6))
	      (catch 'tag
		(while (<= i 5)
		  (condition-case nil
		      (and
		       (ad-Orig-require (ad-get-arg 0) (format "%s~%d" str i))
		       (throw 'tag t))
		    (error nil))
		  (setq i (1+ i)))))
	     (t
	      nil)))
     (or (featurep (ad-get-arg 0))
	 (error (car err) (cdr err))))))

;; Functions.
(or (fboundp 'make-color-instance)
    (defalias 'make-color-instance 'ignore))
(or (fboundp 'color-instance-rgb-components)
    (defalias 'color-instance-rgb-components 'ignore))

;;
(provide 'skk-autoloads)
(provide 'skk-dos)

;;; skk-dos.el ends here
