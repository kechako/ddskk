;;; skk-dos.el --- MS-DOS related codes for skk.el

;; Copyright (C) 1999, 2000 Tsukamoto Tetsuo

;; Author: Tsukamoto Tetsuo <czkmt@remus.dti.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-dos.el,v 1.2 2000/10/30 22:18:14 minakaji Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2000/10/30 22:18:14 $

;; This file is not part of Daredevil SKK yet.

;; Daredevil SKK  is free software;  you  can redistribute it  and/or modify it
;; under the terms  of the GNU General Public License  as published by the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; Daredevil SKK is distributed in the hope that it will be useful  but WITHOUT
;; ANY  WARRANTY;  without  even  the implied  warranty  of MERCHANTABILITY  or
;; FITNESS  FOR  A PARTICULAR PURPOSE.  See the GNU General Public License  for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; Daredevil SKK,  see the file COPYING.  If not,  write  to  the Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:

;; $B$3$l$O(B Daredevil SKK $B$r(B DOS $BMQ(B Emacs $B$GF0$+$9$?$a$N(B work around $B$G$9!#%U%!%$(B
;; $B%kL>$N@)8B$r6/0z$K2r7h$9$k$3$H$rL\E*$H$7$F$$$^$9(B  ($B4D6-$K$h$C$F$O(B  long file
;; name $B$,07$($k$h$&$G$9$,(B)$B!#(B
;; DOS $BMQ(B  Mule $B$O(B window-system $B<~$j$KFC<l$J<BAu$,$5$l$F$$$k$N$G$=$NJU$bB?>/$4(B
;; $B$^$+$7$^$9!#(B
;; $B%$%s%9%H!<%k$^$G$O%5%]!<%H$7$F$$$^$;$s!#(BAPEL $B$H(B SKK $B$N(B Emacs Lisp $B%3!<%I$r9%(B
;; $B$-$J=j$K%3%T!<$7$F(B load-path $B$r@_Dj$7$F$/$@$5$$!#$=$N8e(B ~/_emacs $B$K(B
;;
;; (require 'skk-dos)
;;
;; $B$H=q$$$F$/$@$5$$!#$=$NB>$N@_Dj$O(B info $B$r;2>H$7$F$/$@$5$$!#(B
;;
;; $B$b$7$3$N%U%!%$%k$r%m!<%I$7$F$$$kESCf$G%(%i!<$,H/@8$7$?>l9g$O!"(B *skk-dos-err*
;; $B$H$$$&L>A0$N%P%C%U%!$,$G$-$F$=$3$K%(%i!<$NFbMF$,=q$-$3$^$l$^$9!#(B
;;
;; -*- Demacs $B$G$NCm0U(B -*-
;;
;; cl.el $B$J$I!"%*%j%8%J%k$N(B Nemacs $B$N(B Emacs Lisp $B%U%!%$%k$r%$%s%9%H!<%k$7$F$*$$(B
;; $B$F2<$5$$!#(B skk-dos.el $B$HF1$8$/(B experimental $B%G%#%l%/%H%j$K$"$k(B skk-e18.el $B$b(B
;; $BI,MW$J$N$G%$%s%9%H!<%k$7$^$9!#$^$?!"(BTMP $B4D6-JQ?t$O(B
;;
;; set TMP=c:\tmp
;;
;; $B$N$h$&$K=q$+$:!"(B
;;
;; set TMP=c:/tmp
;;
;; $B$N$h$&$K=q$$$F2<$5$$!#$5$b$J$1$l$P!"(B~/tmp $B$H$$$&%G%#%l%/%H%j$r0l;~%G%#%l%/%H(B
;; $B%j$H$7$F;H$$$^$9$N$G:n$C$F$*$$$F2<$5$$!#(B

;;; Code:

(require 'cl)

;; Autoloads.
(cond
 ((stringp
   (catch 'answer
     (mapcar
      '(lambda (dir)
	 (mapcar
	  '(lambda (suf)
	     (let ((try (expand-file-name (concat "skk-gadg" suf) dir)))
	       (and (file-readable-p try)
		    (null (file-directory-p try))
		    (throw 'answer try))))
	  '(".elc" ".el" "")))
      load-path)))
  ;;
  (autoload 'skk-abbrev-search "skk-abbr" nil nil nil)
  (autoload 'skk-ad-to-gengo "skk-gadg" nil nil nil)
  (autoload 'skk-adjust-search-prog-list-for-auto-okuri "skk-auto" nil nil nil)
  (autoload 'skk-auto-fill-mode "skk" nil t nil)
  (autoload 'skk-calc "skk-gadg" nil nil nil)
  (autoload 'skk-clock "skk-gadg" nil t nil)
  (autoload 'skk-compile-rule-list "skk" nil nil nil)
  (autoload 'skk-completion "skk-comp" nil nil nil)
  (autoload 'skk-current-date "skk-gadg" nil nil nil)
  (autoload 'skk-display-code-for-char-at-point "skk-kcod" nil t nil)
  (autoload 'skk-gengo-to-ad "skk-gadg" nil nil nil)
  (autoload 'skk-henkan-face-off-and-remove-itself "skk-gadg" nil nil nil)
  (autoload 'skk-ignore-dic-word "skk-gadg" nil nil nil)
  (autoload 'skk-input-by-code-or-menu "skk-kcod" nil t nil)
  (autoload 'skk-isearch-mode-cleanup "skk-isea" nil nil nil)
  (autoload 'skk-isearch-mode-setup "skk-isea" nil nil nil)
  (autoload 'skk-jisx0201-mode "skk-jisx" nil t nil)
  (autoload 'skk-minus "skk-gasg" nil nil nil)
  (autoload 'skk-mode "skk" nil t nil)
  (autoload 'skk-num "skk-num" nil nil nil)
  (autoload 'skk-num-compute-henkan-key "skk-num" nil nil nil)
  (autoload 'skk-num-henkan-key "skk-num" nil nil nil)
  (autoload 'skk-num-initialize "skk-num" nil nil nil)
  (autoload 'skk-num-process-user-minibuf-input "skk-num" nil nil nil)
  (autoload 'skk-num-uniq "skk-num" nil nil nil)
  (autoload 'skk-num-update-jisyo "skk-num" nil nil nil)
  (autoload 'skk-obsolete-check "skk-obso" nil t nil)
  (autoload 'skk-obsolete-check-all-files "skk-obso" nil t nil)
  (autoload 'skk-obsolete-put-obsolete-mark "skk-obso" nil nil nil)
  (autoload 'skk-okuri-search "skk-auto" nil nil nil)
  (autoload 'skk-plus "skk-gadget" nil nil nil)
  (autoload 'skk-previous-completion "skk-comp" nil nil nil)
  (autoload 'skk-start-henkan-with-completion "skk-comp" nil t nil)
  (autoload 'skk-study-read "skk-stud" nil t nil)
  (autoload 'skk-study-save "skk-stud" nil t nil)
  (autoload 'skk-study-search "skk-stud" nil nil nil)
  (autoload 'skk-study-update "skk-stud" nil nil nil)
  (autoload 'skk-submit-bug-report "skk-deve" nil t nil)
  (autoload 'skk-times "skk-gadg" nil nil nil)
  (autoload 'skk-today "skk-gadg" nil t nil)
  (autoload 'skk-toggle-katakana "skk-jisx" nil t nil)
  (autoload 'skk-tutorial "skk-tut" nil t nil)
  (autoload 'skk-version "skk" nil t nil)
  (autoload 'skk-viper-normalize-map "skk-vipe" nil t nil))
 (t
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
  (autoload 'skk-viper-normalize-map "skk-vi~1" nil t nil)))

;;

(defconst emacs-major-version
  (progn (string-match "^[0-9]+" emacs-version)
	 (string-to-int (substring emacs-version
				   (match-beginning 0)(match-end 0))))
  "Major version number of this version of Emacs.")
(defconst emacs-minor-version
  (progn (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
	 (string-to-int (substring emacs-version
				   (match-beginning 1)(match-end 1))))
  "Minor version number of this version of Emacs.")

(defun skk-dos-load-apel ()
  (cond ((= emacs-major-version 18)
	 ;; Demacs 1.2.0
	 (defun accept-process-output (&rest args))
	 (provide 'static)
	 (provide 'filename)
	 (require 'product)
	 (load "static")
	 (require 'poe-18)
	 (unless (fboundp 'open-network-stream)
	   (condition-case nil
	       (require 'tcp)
	     (error
	      (provide 'tcp))))
	 (condition-case nil
	     (progn
	       (require 'localhook "localhoo")
	       (require 'pces-nemacs "pces-nem")
	       (require 'poem-nemacs "poem-nem")
	       (require 'poem)
	       (require 'tinycustom "tinycust")
	       (require 'pcustom)
	       (require 'mcs-nemacs "mcs-nema")
	       (require 'invisible "invisibl")
	       (require 'path-util "path-uti"))
	   (error
	    (require 'localhook "localh~1")
	    (require 'pces-nemacs "pces-n~1")
	    (require 'poem-nemacs "poem-n~1")
	    (require 'poem)
	    (require 'tinycustom "tinycu~1")
	    (require 'pcustom)
	    (require 'mcs-nemacs "mcs-ne~1")
	    (require 'invisible "invisi~1")
	    (require 'path-util "path-u~1")))
	 (require 'emu)
	 (load "filename"))
	(t
	 ;; Mule 2.3 based on Emacs 19.30
	 (condition-case nil
	     (progn
	       (require 'invisible "invisibl")
	       (require 'path-util "path-uti"))
	   (error
	    (require 'invisible "invisi~1")
	    (require 'path-util "path-u~1")))
	 (add-hook 'skk-load-hook
		   '(lambda ()
		      (require 'skk-cursor "skk-cu~1")))
	 ;;
	 (defadvice skk-save-jisyo (around skk-dos-ad activate)
	   (let ((mode (file-modes skk-jisyo)))
	     ad-do-it
	     (set-file-modes skk-jisyo mode)))
	 ;;
	 (or (fboundp 'make-color-instance)
	     (defalias 'make-color-instance 'ignore))
	 (or (fboundp 'color-instance-rgb-components)
	     (defalias 'color-instance-rgb-components 'ignore)))))

(condition-case err1
    (skk-dos-load-apel)
  (error
   (save-excursion
     (set-buffer (get-buffer-create "*skk-dos-err*"))
     (insert (format "%s" err1)))
   (signal 'error err1)))

(provide 'skk-autoloads)

(require 'product)
(product-provide (provide 'skk-dos) (require 'skk-version))
;;; skk-dos.el ends here
