;;; skk-vars.el --- variables and constants commonly use
;;    in Daredevil SKK package programs.
;; Copyright (C) 1999, 2000, 2001 SKK Development Team <skk@ring.gr.jp>

;; Author: SKK Development Team <skk@ring.gr.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-vars.el,v 1.76 2001/11/16 01:17:15 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/11/16 01:17:15 $

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

;;; Code:

;; APEL
(require 'path-util) ; for exec-installed-p.

(eval-when-compile

  ;; shut down compiler warnings.
  (defvar word-across-newline)
  (defvar emacs-beta-version)
  (defvar mule-version)

  (defalias-maybe 'frame-property 'ignore)
  (defalias-maybe 'locate-data-file 'ignore)

  (defmacro skk-deflocalvar (var default-value &optional documentation)
    (` (progn
	 (defvar (, var) (, default-value)
	   (, (format "%s\n\(buffer local\)" documentation)))
	 (make-variable-buffer-local '(, var)))))
  (require 'static))

(eval-and-compile
  (defconst skk-emacs-type
    (cond
     ((featurep 'xemacs)
      'xemacs)
     ((string< "5.0" mule-version)
      'mule5)
     ((string< "4.0" mule-version)
      'mule4)
     ((string< "3.0" mule-version)
      'mule3)
     ((string< "2.0" mule-version)
      'mule2)))
  ;;
  (require 'pcustom))

(defconst skk-ml-address "skk@ring.gr.jp")
(defconst skk-ml-command-address "skk-request@ring.gr.jp")
(defconst skk-background-mode
  ;; from font-lock-make-faces of font-lock.el  Welcome!
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (if (< (apply '+ (color-rgb-components
		      (face-property 'default 'background)))
	   (/ (apply '+ (color-rgb-components
			 (make-color-specifier "white"))) 3))
	'dark
      'light))
   (t
    (cond
     ((and window-system (x-display-color-p))
      (let ((bg-resource (x-get-resource ".backgroundMode"
					 "BackgroundMode"))
	    params)
	(if bg-resource
	    (intern (downcase bg-resource))
	  (setq params (frame-parameters))
	  (static-cond
	   ((and (eq system-type 'windows-nt)
		 (fboundp 'win32-color-values))
	    ;; Mule for Windows
	    (< (apply '+ (win32-color-values
			  (cdr (assq 'background-color params))))
	       (/ (apply '+ (win32-color-values "white")) 3))
	    'dark)
	   ((and (eq system-type 'windows-nt)
		 (not (fboundp 'x-color-values)))
	    (if (string-match "light"
			      (cdr (assq 'background-color params)))
		'light
	      'dark))
	   (t
	    (cond
	     ((cdr (assq 'background-mode params)));; Emacs20.x (Meadow)
	     ((< (apply '+ (x-color-values
			    (cdr (assq 'background-color params))))
		 (/ (apply '+ (x-color-values "white")) 3))
	      'dark)
	     (t 'light)))))))
     (t 'mono)))))

;;;; variables declaration
;;; user variables

(defgroup skk nil "Top of SKK customization group."
  :prefix "skk-"
  :group 'mule
  :group 'leim)

(defgroup skk-custom-by-function nil
  "SKK customization root grouping by function."
  :group 'skk)

(defgroup skk-custom-by-filename nil
  "SKK customization root grouping by filename."
  :group 'skk)

;;; by function
(defgroup skk-decoration nil "Decoration"
  :group 'skk-custom-by-function)

(defgroup skk-dictionary nil "Dictionary"
  :group 'skk-custom-by-function)

(defgroup skk-filenames nil "Filenames"
  :group 'skk-custom-by-function)

(defgroup skk-hooks-and-functions nil "Hooks and Funcalled functions"
  :group 'skk-custom-by-function)

(defgroup skk-keybinds nil "Keybinds"
  :group 'skk-custom-by-function)

(defgroup skk-okurigana nil "Okurigana processing"
  :group 'skk-custom-by-function)

(defgroup skk-misc nil "Miscellaneous"
  :group 'skk-custom-by-function)

;;; by filename
(defgroup skk-abbrev nil "SKK Abbrev mode related customization."
  :group 'skk-custom-by-filename)

(defgroup skk-auto nil "SKK auto okuri process related customization."
  :prefix "skk-"
  :group 'skk-custom-by-filename)

(defgroup skk-comp nil "SKK completion related customization."
  :prefix "skk-"
  :group 'skk-custom-by-filename)

(defgroup skk-cursor nil "SKK cursor related customization."
  :prefix "skk-cursor-"
  :group 'skk-custom-by-filename)

(defgroup skk-dcomp nil "SKK dynamic completion related customization."
  :prefix "skk-dcomp-"
  :group 'skk-custom-by-filename)

(defgroup skk-gadget nil "SKK gadget related customization."
  :prefix "skk-"
  :group 'skk-custom-by-filename)

(defgroup skk-isearch nil "SKK incremental search related customization."
  :prefix "skk-isearch-"
  :group 'skk-custom-by-filename)

(defgroup skk-jisx0201 nil "SKK jisx0201 (Hankaku Kana) related customization."
  :prefix "skk-jisx0201-"
  :group 'skk-custom-by-filename)

(defgroup skk-kakasi nil "SKK kakasi related customization."
  :prefix "skk-"
  :group 'skk-custom-by-filename)

(defgroup skk-kcode nil "SKK Kanji Code related customization."
  :prefix "skk-"
  :group 'skk-custom-by-filename)

(defgroup skk-look nil "SKK look conversion related customization."
  :prefix "skk-look-"
  :group 'skk-custom-by-filename)

(defgroup skk-lookup nil "SKK lookup related customization."
  :prefix "skk-lookup-"
  :group 'skk-custom-by-filename)

(defgroup skk-num nil "SKK number conversion related customization."
  :prefix "skk-num-"
  :group 'skk-custom-by-filename)

(defgroup skk-server nil "SKK server related customization."
  :prefix "skk-server-"
  :group 'skk-custom-by-filename)

(defgroup skk-tut nil "SKK tutorial conversion related customization."
  :prefix "skk-tut-"
  :group 'skk-custom-by-filename)

(defgroup skk-annotation nil "SKK annotation related customization."
  :prefix "skk-annotation-"
  :group 'skk-custom-by-filename)

;;(defgroup skk-viper nil "SKK/Viper related customization."
;;  :prefix "skk-viper-"
;;  :group 'skk-custom-by-filename)

(defcustom skk-init-file (convert-standard-filename "~/.skk")
  "*SKK $B$N=i4|@_Dj%U%!%$%kL>!#(B
$B$3$N%U%!%$%k$NBe$o$j$K(B ~/.emacs $B$G@_Dj$9$k$3$H$b2DG=!#(B"
  ;;"*Name of the SKK initialization file.
  ;;From skk.el 9.x on all customization may be done in ~/.emacs."
  :type '(choice file (const nil))
  :group 'skk-filenames)

(defcustom skk-special-midashi-char-list '(?> ?< ??)
  "*$B@\F,<-!"@\Hx<-$NF~NO$r;XDj$9$kJ8;z$N%j%9%H!#(B"
  ;;  "*List of characters for entering prefixes and suffixes."
  :type '(repeat character)
  :group 'skk-keybinds)

(defcustom skk-mode-hook nil
  "*SKK $B$r5/F0$7$?$H$-$N%U%C%/!#(B
$BB>$K!"(B`skk-auto-fill-mode-hook', `skk-load-hook', `skk-init-file' $B$G$b(B
$B%+%9%?%^%$%:$,2DG=!#(B"
  ;; "*Hook run at SKK startup.  This hook is also run
  ;;in skk-auto-fill-mode after skk-auto-fill-mode-hook.
  ;;skk-auto-fill-mode-hook, skk-load-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk-hooks-and-functions)

(defcustom skk-auto-fill-mode-hook nil
  "*`skk-auto-fill-mode' $B$r5/F0$7$?$H$-$N%U%C%/!#(B
$BB>$K!"(B`skk-mode-hook', `skk-load-hook', `skk-init-file' $B$G$b%+%9%?%^%$%:$,(B
$B2DG=!#(B"
  ;;  "*Hook run at startup of skk-auto-fill-mode.
  ;;skk-mode-hook$B!"(Bskk-load-hook, skk-init-file may also be used for
  ;;customization."
  :type 'hook
  :group 'skk-hooks-and-functions)

(defcustom skk-load-hook nil
  "*skk.el $B$r%m!<%I$7$?$H$-$N%U%C%/!#(B
$BB>$K!"(B`skk-mode-hook', `skk-auto-fill-mode-hook', `skk-init-file' $B$G$b%+%9%?(B
$B%^%$%:$,2DG=!#(B"
  ;;  "*Hook run when SKK is loaded.
  ;;skk-auto-fill-mode-hook$B!"(Bskk-mode-hook, skk-init-file may also be used
  ;;for customization."
  :type 'hook
  :group 'skk-hooks-and-functions)

(defcustom skk-search-end-function nil
  "*$BC18l8!:w=*N;;~$K%3!<%k$5$l$k4X?t!#(B
$B$3$N4X?t$rMxMQ$7$F8!:w$7$?C18l$NM%@h=g0L$rJQ99$9$k$J$I$N:n6H$,2DG=!#(B
HENKAN-BUFFER, MIDASI, OKURIGANA, ENTRY $B$N(B 4 $B0z?t$rH<$J$C$F%3!<%k$5$l$k!#(B
$B2C9)$7$?(B ENTRY $B$rJV$9$3$H!#(B
$B$3$N4X?t$O!"<-=q%P%C%U%!$G%3!<%k$5$l$k$N$G!"JQ49$r9T$C$?%P%C%U%!%m!<%+%k$J(B
$B>pJs$r<h$j=P$7$?$$$H$-$O!"(BHENKAN-BUFFER $B$rMxMQ$9$k!#(B"
  :type '(choice function (const nil))
  :group 'skk-hooks-and-functions)

(defcustom skk-update-end-function nil
  "*$B8D?M<-=q$N99?7=*N;;~$K%3!<%k$5$l$k4X?t!#(B
HENKAN-BUFFER, MIDASI, OKURIGANA, WORD, PURGE $B$N(B 5 $B0z?t$rH<$J$C$F%3!<%k$5$l$k!#(B
$B$3$N4X?t$O!"<-=q%P%C%U%!$G%3!<%k$5$l$k$N$G!"JQ49$r9T$C$?%P%C%U%!%m!<%+%k$J(B
$B>pJs$r<h$j=P$7$?$$$H$-$O!"(BHENKAN-BUFFER $B$rMxMQ$9$k!#(B
`skk-kakutei-initialize' $B$,%3!<%k$5$l$kA0$K$3$N4X?t$,%3!<%k$5$l$k$N$G!":G8e$N(B
$B3NDj$K4X$9$k%U%i%0N`$O!"$3$N4X?t$NCf$+$i;2>H$9$k$3$H$,$G$-$k!#(B"
  :type '(choice function (const nil))
  :group 'skk-hooks-and-functions)

(defcustom skk-kakutei-end-function nil
  "*$B3NDj;~$K%3!<%k$5$l$k4X?t!#(B
KAKUTEI-WORD $B0z?t$rH<$J$C$F!"JQ49$r9T$C$?%P%C%U%!$G%3!<%k$5$l$k!#(B
skk-kakutei-initialize $B$,%3!<%k$5$l$kA0$K$3$N4X?t$,%3!<%k$5$l$k$N$G!":G8e$N3NDj(B
$B$K4X$9$k%U%i%0N`$O!"$3$N4X?t$NCf$+$i;2>H$9$k$3$H$,$G$-$k!#(B"
  :type '(choice function (const nil))
  :group 'skk-hooks-and-functions)

(defcustom skk-kakutei-jisyo nil
  "*$B:G=i$K8!:w$9$k<-=q!#(B
Non-nil $B$G!"$+$D(B `skk-search-prog-list' $B$NMWAG$NCf$K$3$NJQ?t$,;HMQ$5$l$F(B
$B$$$l$P!";XDj$5$l$?<-=q$r8!:w$N$?$a%P%C%U%!$KFI$_9~$_!"8!:w$r9T$&!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
$B3F8+=P$78l$N:G=i$N%(%s%H%j$7$+8!:w$7$J$$(B ($BJ#?t$N%(%s%H%j$,$"$C$F$b(B 2 $BHVL\0J9_$N(B
$B%(%s%H%j$OL5;k$5$l$k(B)$B!#(B
`skk-search-prog-list' $B$NCM$r@_Dj$9$k$3$H$K$h$j!"8!:wBP>]$N<-=q$NJQ99!"8!:w$N(B
$B=g=x$NJQ99$,2DG=!#(B"
  ;;  "*The first dictionary to be searched.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;The keys must be sorted.
  ;;Only the first entry in each key is checked; if several entries are
  ;;present the second and following entries are ignored.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(choice file (const nil))
  :group 'skk-filenames)

(defcustom skk-initial-search-jisyo nil
  "*$B%f!<%6!<<-=q$N8!:w$NA0$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G!"$+$D(B `skk-search-prog-list' $B$NMWAG$NCf$K$3$NJQ?t$,;HMQ$5$l$F(B
$B$$$l$P!";XDj$5$l$?<-=q$r8!:w$N$?$a%P%C%U%!$KFI$_9~$_!"8!:w$r9T$&!#(B
`skk-search-prog-list' $B$NCM$r@_Dj$9$k$3$H$K$h$j!"8!:wBP>]$N<-=q$NJQ99!"8!:w$N(B
$B=g=x$NJQ99$,2DG=!#(B"
  ;;  "*This dictionary is searched before the user's personal dictionary.
  ;;The keys must be sorted.
  ;;If non-nil, and this variable is used as a component of
  ;;`skk-search-prog-list', the indicated dictionary is read into a
  ;;buffer and searched.
  ;;By setting the value of `skk-search-prog-list' the dictionaries
  ;;searched and the order of search can be changed."
  :type '(choice file (const nil))
  :group 'skk-filenames)

(defcustom skk-large-jisyo nil
  "*$B%f!<%6!<<-=q$N8!:w$N8e$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G!"$+$D(B `skk-search-prog-list' $B$NMWAG$NCf$K$3$NJQ?t$,;HMQ$5$l$F(B
$B$$$l$P!";XDj$5$l$?<-=q$r8!:w$N$?$a%P%C%U%!$KFI$_9~$_!"8!:w$r9T$&!#(B
`skk-search-prog-list' $B$NCM$r@_Dj$9$k$3$H$K$h$j!"8!:wBP>]$N<-=q$NJQ99!"8!:w$N(B
$B=g=x$NJQ99$,2DG=!#(B"
  :type '(choice file (const nil))
  :group 'skk-filenames)

(defcustom skk-aux-large-jisyo nil
  "*SKK $B%5!<%P!<$G:G8e$K8!:w$9$k<-=q!#(B
$B8+=P$78l$O!"%=!<%H$5$l$F$$$J$1$l$P$J$i$J$$!#(B
Non-nil $B$G!"$+$D(B `skk-search-prog-list' $B$NMWAG$NCf$K$3$NJQ?t$,;HMQ$5$l$F(B
$B$$$l$P!"(BSKK $B%5!<%P!<$r;H$$8!:w$r9T$&!#(B
SKK $B%5!<%P!<$,(B active $B$G$J$1$l$P!";XDj$5$l$?<-=q$r%P%C%U%!$KFI$_9~$`!#(B
`skk-search-prog-list' $B$NCM$r@_Dj$9$k$3$H$K$h$j!"8!:wBP>]$N<-=q$NJQ99!"(B
$B8!:w$N=g=x$NJQ99$,2DG=!#(B
$B$3$NCM$r@_Dj$9$k$3$H$K$h$j!"(Bskk-server.el $B$,(B autoload $B$5$l$k!#(B"
  :type '(choice file (const nil))
  :group 'skk-filenames)

(defcustom skk-search-prog-list
  '((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
    (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
    (skk-search-jisyo-file skk-jisyo 0 t)
    (skk-search-small-dic)
    (skk-okuri-search)
    (skk-search-jisyo-file skk-large-jisyo 10000)
    (skk-search-server skk-aux-large-jisyo 10000))
  "*$B8!:w4X?t!"8!:wBP>]$N<-=q$r7hDj$9$k$?$a$N%j%9%H!#(B
$BJQ49$7$?8uJd$rJV$9(B S $B<0$r%j%9%H$N7A$KI=5-$7$?$b$N!#(B
`skk-search' $B4X?t$,(B `skk-search-prog-list' $B$N(B car $B$+$i8eJ}8~$X=gHV$K(B S $B<0$N(B
$BI>2A$r9T$$JQ49$r9T$&!#(B"
  :type '(repeat
	  (list (function :tag "Search funcition")
		(choice :tag "Dictionary" file (const nil))
		(choice :tag "Minimum region size to be binary-searched"
			integer (const nil))
		(choice :tag "Quietly reading dictionary to Emacs buffer"
			(const t) (const nil))))
  :group 'skk-dictionary)

(defcustom skk-jisyo (convert-standard-filename "~/.skk-jisyo")
  "*SKK $B$N%f!<%6!<<-=q!#(B"
  :type 'file
  :group 'skk-filenames)

(defcustom skk-backup-jisyo (convert-standard-filename "~/.skk-jisyo.BAK")
  "*SKK $B$N%f!<%6!<<-=q$N%P%C%/%"%C%W%U%!%$%k!#(B"
  :type 'file
  :group 'skk-filenames)

(defcustom skk-jisyo-code nil
  "*Non-nil $B$G$"$l$P!"$=$NCM$G<-=q%P%C%U%!$N4A;z%3!<%I$r@_Dj$9$k!#(B
Mule $B$G$O!"(B*euc-japan*, *sjis*, *junet*$B!#(B
$B$^$?!"(B\"euc\", \"ujis\", \"sjis\", \"jis\" $B$J$I$NJ8;zNs$K$h$C$F$b;XDj$,2DG=!#(B"
  :type '(choice symbol string)
  :group 'skk-dictionary)

(defcustom skk-keep-record t
  "*Non-nil $B$G$"$l$P!"JQ49$K4X$9$k5-O?$r(B `skk-record-file' $B$K<h$k!#(B
$B$?$@$7?tCM$G$"$l$P!"(B`skk-record-file' $B$r$=$N9T?t$h$jBg$-$/$7$J$$!#(B
nil $B$G$"$l$P!"JQ49$K4X$9$k5-O?$r<h$i$J$$!#(B"
  :type '(choice integer (const t) (const nil))
  :group 'skk-misc)

(defcustom skk-record-file (convert-standard-filename "~/.skk-record")
  "*$B%f!<%6!<<-=q$NE}7W$r<h$k%U%!%$%k!#(B
$B<-=q%;!<%V$N;~9o!"C18l$NEPO??t!"3NDj$r9T$C$?2s?t!"3NDjN(!"A4BN$N8l?t$N(B
$B>pJs$r<}$a$k!#(B"
  :type 'file
  :group 'skk-filenames)

(defcustom skk-kakutei-key "\C-j"
  "*$B4A;zJQ49$N3NDjF0:n$r9T$&%-!<!#(B"
  :type 'sexp
  :group 'skk-keybinds)

(defcustom skk-previous-candidate-char ?x
  "*skk-previous-candidate $B$r3dEv$F$?%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-keybinds)

(defcustom skk-try-completion-char ?\011 ; TAB
  "*$B8+=P$78l$NJd40F0:n$r9T$&%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-keybinds)

(defcustom skk-next-completion-char ?.
  "*$B8+=P$78l$NJd40F0:n$G!"<!$N8uJd$r=PNO$9$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-keybinds)

(defcustom skk-previous-completion-char ?,
  "*$B8+=P$78l$NJd40F0:n$G!"A0$N8uJd$r=PNO$9$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-keybinds)

(defcustom skk-start-henkan-char ?\040	; SPC
  "*$B4A;zJQ49$r3+;O$9$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-keybinds)

(defcustom skk-start-henkan-with-completion-char ?\240 ; M-SPC
  "*$B8+=P$78l$rJd40$7$J$,$i"'%b!<%I$KF~$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-keybinds)

(defcustom skk-backward-and-set-henkan-point-char ?\321 ; M-Q
  "*$B%]%$%s%H$rLa$7$F"&%b!<%I$KF~$k%-!<%-%c%i%/%?!#(B"
  :type 'character
  :group 'skk-keybinds)

(defcustom skk-use-viper nil
  "*Non-nil $B$G$"$l$P!"(BVIPER $B$KBP1~$9$k!#(B"
  :type 'boolean
  :group 'skk-viper)

(defcustom skk-henkan-okuri-strictly nil
  "*Non-nil $B$G$"$l$P!"8+=P$78l$HAw$j2>L>$,0lCW$7$?$H$-$@$18uJd$H$7$F=PNO$9$k!#(B
$BNc$($P!"2<5-$N$h$&$J<-=q%(%s%H%j$,!"(B`skk-jisyo' ($B%W%i%$%Y!<%H<-=q(B) $B$K$"$C$?(B
$B>l9g$K(B

  \"$B$*$*(Bk /$BBg(B/$BB?(B/[$B$/(B/$BB?(B/]/[$B$-(B/$BBg(B/]/\"

\"$B"&$*$*(B*$B$/(B\" $B$rJQ49$7$?$H$-!"(B\"$BB?$/(B\" $B$N$_$r=PNO$7!"(B\"$BBg$/(B\" $B$r=PNO$7$J$$!#(B

SKK-JISYO.[SML] $B$NAw$j2>L>%(%s%H%j$O>e5-$N7A<0$K$J$C$F$$$J$$$N$G!"(B`skk-jisyo'
 $B$NAw$j$"$j$N<-=q%(%s%H%j$,$3$N7A<0$N$b$N$r$"$^$j4^$s$G$$$J$$>l9g$O!"$3$N(B
$B%*%W%7%g%s$r(B on $B$K$9$k$3$H$G!"$9$0$KC18lEPO?$KF~$C$F$7$^$&$N$GCm0U$9$k$3$H!#(B

`skk-process-okuri-early' $B$NCM$,(B nil $B$J$i$P>e5-$N7A<0$G(B `skk-jisyo' $B$,(B
$B:n$i$l$k!#(B

Emacs 19 $B0J>e$J$i$P!"2<5-$N<0$rI>2A$9$k$3$H$G!"C18lEPO?$KF~$C$?$H$-$@$1(B
$B0l;~E*$K$3$N%*%W%7%g%s$rL58z$K$9$k$3$H$,$G$-$k!#(B

    (add-hook 'minibuffer-setup-hook
              (function
               (lambda ()
                 (if (and (boundp 'skk-henkan-okuri-strictly)
                          skk-henkan-okuri-strictly
                          (not (eq last-command 'skk-purge-from-jisyo)))
                     (progn
                       (setq skk-henkan-okuri-strictly nil)
                       (put 'skk-henkan-okuri-strictly 'temporary-nil t))))))

    (add-hook 'minibuffer-exit-hook
              (function
               (lambda ()
                 (if (and (get 'skk-henkan-okuri-strictly 'temporary-nil)
                          (<= (minibuffer-depth) 1))
                     (progn
                       (put 'skk-henkan-okuri-strictly 'temporary-nil nil)
                       (setq skk-henkan-okuri-strictly t))))))

$B$3$N%*%W%7%g%sMxMQ;~$O!"(B`skk-process-okuri-early' $B$NCM$O(B nil $B$G$J$1$l$P(B
$B$J$i$J$$!#(B"
  :type 'boolean
  :group 'skk-okurigana)

(defcustom skk-henkan-strict-okuri-precedence nil
  "*Non-nil $B$G$"$l$P!"8+=P$78l$HAw$j2>L>$,0lCW$7$?8uJd$rM%@h$7$FI=<($9$k!#(B
$BNc$($P!"2<5-$N$h$&$J<-=q%(%s%H%j$,!"(B`skk-jisyo' ($B%W%i%$%Y!<%H<-=q(B) $B$K$"$C$?(B
$B>l9g$K(B

  \"$B$*$*(Bk /$BBg(B/$BB?(B/[$B$/(B/$BB?(B/]/[$B$-(B/$BBg(B/]/\"

\"$B"&$*$*(B*$B$/(B\" $B$rJQ49$7$?$H$-!"$^$:(B\"$BB?$/(B\" $B$r=PNO$7!"(B
$B<!$K(B \"$BBg$/(B\" $B$r=PNO$9$k!#(B

\"$BBg$/(B\"$B$J$I$N8uJd$O$&$C$H$&$7$$$,!"$9$0$KC18lEPO?$K$O$$$C$F$7$^$&$N$b(B
$B7y$J$R$H$K$*$9$9$a!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(B`skk-process-okuri-early' $B$NCM$O(B nil $B$G$J$1$l$P(B
$B$J$i$J$$!#(B
$B$^$?(B `skk-henkan-okuri-strictly' $B$,(B non-nil $B$N$H$-$O!"$3$NJQ?t$OL5;k$5$l$k!#(B"
  :type 'boolean
  :group 'skk-okurigana)

(defcustom skk-auto-okuri-process nil
  "*Non-nil $B$G$"$l$P!"Aw$j2>L>ItJ,$r<+F0G'<1$7$FJQ49$r9T$&!#(B
$BNc$($P!"(B

    \"Uresii (\"UreSii\" $B$G$O$J$/(B) -> $B4r$7$$(B\"

$B$N$h$&$KJQ49$5$l$k!#C"$7!"(Bskk-jisyo $B<-=q(B \($B%W%i%$%Y!<%H<-=q(B\) $B$,!"(B

    \"$B$&$l(Bs /$B4r(B/[$B$7(B/$B4r(B/]/\"

$B$N$h$&$J7A<0$K$J$C$F$$$k$3$H$,I,MW$G$"$k(B (SKK-JISYO.[SML] $B$O$3$N7A<0$KBP1~$7(B
$B$F$$$J$$$N$G!"(B`skk-jisyo' $B$K$3$N%(%s%H%j$,$J$1$l$P$J$i$J$$(B)$B!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(B`skk-process-okuri-early' $B$NCM$O(B nil $B$G$J$1$l$P(B
$B$J$i$J$$!#(B"
  :type 'boolean
  :group 'skk-okurigana)

(defcustom skk-process-okuri-early nil
  "*Non-nil $B$G$"$l$PAw$j2>L>$N%m!<%^;z%W%l%U%#%C%/%9F~NO;~E@$GJQ49$r3+;O$9$k!#(B
$BNc$($P!"(B

    \"UgoK -> $B"'F0(Bk\"$B!#(B

$BAw$j2>L>$,J,$i$J$$$^$^JQ49$7$F$$$k$3$H$K$J$k$N$G!"(B`skk-jisyo' $B$,Aw$j2>L>$K(B
$BBP1~$7$?7A$K@.D9$7$J$$!#$D$^$j(B

    \"$B$&$4(Bk /$BF0(B/\"

$B$N$h$&$J7ABV$N$^$^$H$J$k!#$?$@$7!"4{$K(B

    \"$B$&$4(Bk /$BF0(B/[$B$/(B/$BF0(B/]/[$B$+(B/$BF0(B/]/[$B$1(B/$BF0(B/]/[$B$-(B/$BF0(B/]/[$B$3(B/$BF0(B/]/\"

$B$N$h$&$J%(%s%H%j$,(B skk-jisyo $B$K$"$l$P!"$=$l$rGK2u$7$J$$!#(B

nil $B$G$"$l$P!"Aw$j2>L>$NF~NO$,40N;$7$?;~E@$GJQ49$,3+;O$9$k!#Nc$($P!"(B

    \"UgoK -> $B"&$&$4(B*k\", \"UgoKu -> $B"'F0$/(B\"

$B$3$N%*%W%7%g%s$r(B on $B$K$7$F(B skk-mode $B$r5/F0$9$k$H!"N>N)$G$-$J$$%*%W%7%g%s$G$"$k(B
`skk-kakutei-early', `skk-auto-okuri-process' $B$*$h$S(B
`skk-henkan-okuri-strictly' $B$O(B nil $B$K%;%C%H$5$l$k!#(B"
  :type 'boolean
  :group 'skk-okurigana)

(defcustom skk-egg-like-newline nil
  "*Non-nil $B$G$"$l$P!""'%b!<%I$G2~9T$r%?%$%W$7$F$b3NDj$9$k$N$_$G2~9T$7$J$$!#(B"
  :type 'boolean
  :group 'skk-keybinds)

(defcustom skk-kakutei-early t
  "*Non-nil $B$G$"$l$P(B `skk-insert' $B$,8F$P$l$?$H$-$K8=:_$N8uJd$r3NDj$9$k!#(B
$BNc$($P!"(B

    \"$B"&$+$/$F$$(B -> $B"'3NDj(B -> $B3NDj(Bs -> $B3NDj$9(B\"

$B$N$h$&$KJQ498e!"!V$9!W$N(B prefix $B$G$"$k(B \"s\" $B$rF~NO$7$?;~E@$G3NDj$9$k!#(B
nil $B$G$"$l$P!"Nc$($P(B

    \"$B"&$+$/$F$$(B -> $B"'3NDj(B -> $B"'3NDj(Bs -> $B"'3NDj$9$k(B -> $B3NDj$9$k!#(B\"

$B$N$h$&$K(B `skk-kakutei' $B$rD>@\!"4V@\$K%3!<%k$9$k$^$G(B ($B6gFIE@$rF~NO$7$?$j!"(B
$B?7$?$J"&%b!<%I$KF~$C$?$j$9$k$H4V@\E*$K(B `skk-kakutei $B$r%3!<%k$9$k(B) $B$O!"3NDj(B
$B$7$J$$$N$G!"$=$N4V$O!"JQ498uJd$rA*$S$J$*$9$3$H$J$I$,2DG=!#(B

$B$3$N%*%W%7%g%sMxMQ;~$O!"(B`skk-process-okuri-early' $B$NCM$O(B nil $B$G$J$1$l$P(B
$B$J$i$J$$!#(B"
  :type 'boolean
  :group 'skk-keybinds)

(defcustom skk-delete-implies-kakutei t
  "*Non-nil $B$G$"$l$P!""'%b!<%I$G(B BS $B$r2!$9$H!"A0$N0lJ8;z$r:o=|$73NDj$9$k!#(B
nil $B$G$"$l$P!"0l$DA0$N8uJd$rI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-keybinds)

(defcustom skk-allow-spaces-newlines-and-tabs t
  "*Non-nil $B$G$"$l$P!"8+=P$78l$NCf$N%9%Z!<%9!"%?%V$r<h$j=|$$$FJQ49$G$-$k!#(B
$BNc$($P!"2<5-$N$h$&$K(B $BCf$K2~9T$,F~$C$F$$$F$bJQ49$,2DG=$G$"$k!#(B

     \"$B"&$+(B
  $B$J(B\"
   -> \"$B2>L>(B\"

$B$3$NCM$,(B nil $B$G$"$l$P!":G=i$N%9%Z!<%9$G8+=P$78l$r@Z$j5M$a$F$7$^$$!"0J9_$N%9%Z!<(B
$B%9!"%?%V!"2~9T$OL5;k$5$l$k!#(B
$B$3$NCM$O!"(B`skk-start-henkan', `skk-latin-henkan', `skk-katakana-henkan',
`skk-hiragana-henkan', `skk-jisx0208-latin-henkan' $B5Z$S(B
`skk-backward-and-set-henkan-point' $B$NF0:n$K1F6A$9$k!#(B"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-delete-okuri-when-quit nil
  "*Non-nil $B$G$"$l$PJQ49Cf$N(B\\[keyboard-quit]$B$GAw$j2>L>$r>C$7$F"&%b!<%I$KF~$k!#(B
$BNc$($P!"(B

    \"$B"&$J(B*$B$/(B -> $B"'5c$/(B -> \\[keyboard-quit] ->$B"&$J(B\"

nil $B$G$"$l$P!"Aw$j2>L>$r4^$a$?8+=P$78l$r$=$N$^$^;D$7!""#%b!<%I$KF~$k!#Nc$($P!"(B

    \"$B"&$J(B*$B$/(B -> $B"'5c$/(B -> \\[keyboard-quit] -> $B$J$/(B\""
  :type 'boolean
  :group 'skk-keybinds)

(defcustom skk-check-okurigana-on-touroku nil
  "*Non-nil $B$G$"$l$P!"Aw$j$"$j$NEPO?;~$K!"M>7W$J2>L>$r%A%'%C%/$9$k!#(B

$BNc$($P!"(B

     \"$B$H$S$@(B*$B$9(B $BHt$S=P(B\"

$B$HEPO?$9$k$N$,@5$7$$$K$b$+$+$o$i$:!"%f!<%6$,(B

     \"$B$H$S$@(B*$B$9(B $BHt$S=P$9(B\"

$B$G$&$C$+$j(B [RET] $B$r2!$7$F$7$^$C$?$H$-$K!":G8e$N!V$9!W$,Aw$j2>L>$G$"$k$+$I$&$+(B
$BD4$Y$k!#(B

$B$3$NJQ?t$O0J2<$NCM$r$H$jF@$k!#(B

ask  -- $B%f!<%6$K3NG'$r5a$a!"Aw$j2>L>$HG'$a$i$l$l$P$3$l$r<h$j=|$$$F$+$iEPO?$9(B
        $B$k!#(B
auto -- $B%f!<%6$K3NG'$r5a$a$:!">!<j$KAw$j2>L>$rH=CG$7$F:o=|$7$F$+$iEPO?$9$k!#(B
nil  -- $B0l@ZAw$j2>L>$N%A%'%C%/$r$;$:!"A4BN$rC18l$H$7$FEPO?$9$k!#$3$l$O(B SKK $BK\(B
        $BMh$NF0:n$G$"$j!"=>$C$F$3$NJQ?t$N4{DjCM$O(B nil $B$G$"$k!#(B"
  :type '(choice (const auto)
		 (const ask)
		 (const nil))
  :group 'skk-okurigana)

(defcustom skk-henkan-show-candidates-keys '(?a ?s ?d ?f ?j ?k ?l)
  "*$B%a%K%e!<7A<0$G8uJd$rA*Br$9$k$H$-$NA*Br%-!<$N%j%9%H!#(B
\"x\", \" \" $B5Z$S(B \"C-g\" $B0J30$N(B 7 $B$D$N%-!<(B (char type) $B$r4^$`I,MW$,$"(B
$B$k!#(B\"x\", \" \" $B5Z$S(B \"C-g\" $B$O8uJdA*Br;~$K$=$l$>$lFCJL$J;E;v$K3d$jEv(B
$B$F$i$l$F$$$k$N$G!"$3$N%j%9%H$NCf$K$O4^$a$J$$$3$H!#(B"
  :type '(repeat character)
  :group 'skk-keybinds)

(defcustom skk-status-indicator 'left
  "*SKK $B$N>uBV$r%b!<%I9T$N$I$3$KI=<($9$k$+$r7h$a$k!#(B
left $B$G$"$l$P:8C<$KI=<($9$k!#(B
$B$5$b$J$1$l$P%^%$%J!<%b!<%I$H$7$F$NI=<(K!$r<h$k!#(B"
  :type '(choice (const minor-mode)
		 (const left)
		 symbol)
  :group 'skk-decoration)

(defcustom skk-latin-mode-string "SKK"
  "*SKK $B$,(B latin (ascii) $B%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-decoration)

(defcustom skk-hiragana-mode-string "$B$+$J(B"
  "*$B$R$i$,$J%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-decoration)

(defcustom skk-katakana-mode-string "$B%+%J(B"
  "*$B%+%?%+%J%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-decoration)

(defcustom skk-jisx0208-latin-mode-string "$BA41Q(B"
  "*$BA41Q%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-decoration)

(defcustom skk-abbrev-mode-string "a$B$"(B"
  "*SKK abbrev $B%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-decoration)

(defcustom skk-indicator-use-cursor-color (and window-system
					       (fboundp 'x-display-color-p)
					       (x-display-color-p))
  "*Non-nil $B$J$i$P%+!<%=%k$HF1$8?'$G%$%s%8%1!<%?$rI=<($9$k(B"
  :type 'boolean
  :group 'skk-decoration)

(defcustom skk-echo t
  "*Non-nil $B$G$"$l$P!"2>L>J8;z$N%W%l%U%#%C%/%9$rI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-decoration)

(defcustom skk-use-numeric-conversion t
  "*Non-nil $B$G$"$l$P!"?tCMJQ49$r9T$&!#(B"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-rom-kana-base-rule-list
  '(("a" nil ("$B%"(B" . "$B$"(B"))
    ("bb" "b" ("$B%C(B" . "$B$C(B"))
    ("ba" nil ("$B%P(B" . "$B$P(B"))
    ("be" nil ("$B%Y(B" . "$B$Y(B"))
    ("bi" nil ("$B%S(B" . "$B$S(B"))
    ("bo" nil ("$B%\(B" . "$B$\(B"))
    ("bu" nil ("$B%V(B" . "$B$V(B"))
    ("bya" nil ("$B%S%c(B" . "$B$S$c(B"))
    ("bye" nil ("$B%S%'(B" . "$B$S$'(B"))
    ("byi" nil ("$B%S%#(B" . "$B$S$#(B"))
    ("byo" nil ("$B%S%g(B" . "$B$S$g(B"))
    ("byu" nil ("$B%S%e(B" . "$B$S$e(B"))
    ("cc" "c" ("$B%C(B" . "$B$C(B"))
    ("cha" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("che" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("chi" nil ("$B%A(B" . "$B$A(B"))
    ("cho" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("chu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("cya" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("cye" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("cyi" nil ("$B%A%#(B" . "$B$A$#(B"))
    ("cyo" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("cyu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("dd" "d" ("$B%C(B" . "$B$C(B"))
    ("da" nil ("$B%@(B" . "$B$@(B"))
    ("de" nil ("$B%G(B" . "$B$G(B"))
    ("dha" nil ("$B%G%c(B" . "$B$G$c(B"))
    ("dhe" nil ("$B%G%'(B" . "$B$G$'(B"))
    ("dhi" nil ("$B%G%#(B" . "$B$G$#(B"))
    ("dho" nil ("$B%G%g(B" . "$B$G$g(B"))
    ("dhu" nil ("$B%G%e(B" . "$B$G$e(B"))
    ("di" nil ("$B%B(B" . "$B$B(B"))
    ("do" nil ("$B%I(B" . "$B$I(B"))
    ("du" nil ("$B%E(B" . "$B$E(B"))
    ("dya" nil ("$B%B%c(B" . "$B$B$c(B"))
    ("dye" nil ("$B%B%'(B" . "$B$B$'(B"))
    ("dyi" nil ("$B%B%#(B" . "$B$B$#(B"))
    ("dyo" nil ("$B%B%g(B" . "$B$B$g(B"))
    ("dyu" nil ("$B%B%e(B" . "$B$B$e(B"))
    ("e" nil ("$B%((B" . "$B$((B"))
    ("ff" "f" ("$B%C(B" . "$B$C(B"))
    ("fa" nil ("$B%U%!(B" . "$B$U$!(B"))
    ("fe" nil ("$B%U%'(B" . "$B$U$'(B"))
    ("fi" nil ("$B%U%#(B" . "$B$U$#(B"))
    ("fo" nil ("$B%U%)(B" . "$B$U$)(B"))
    ("fu" nil ("$B%U(B" . "$B$U(B"))
    ("fya" nil ("$B%U%c(B" . "$B$U$c(B"))
    ("fye" nil ("$B%U%'(B" . "$B$U$'(B"))
    ("fyi" nil ("$B%U%#(B" . "$B$U$#(B"))
    ("fyo" nil ("$B%U%g(B" . "$B$U$g(B"))
    ("fyu" nil ("$B%U%e(B" . "$B$U$e(B"))
    ("gg" "g" ("$B%C(B" . "$B$C(B"))
    ("ga" nil ("$B%,(B" . "$B$,(B"))
    ("ge" nil ("$B%2(B" . "$B$2(B"))
    ("gi" nil ("$B%.(B" . "$B$.(B"))
    ("go" nil ("$B%4(B" . "$B$4(B"))
    ("gu" nil ("$B%0(B" . "$B$0(B"))
    ("gya" nil ("$B%.%c(B" . "$B$.$c(B"))
    ("gye" nil ("$B%.%'(B" . "$B$.$'(B"))
    ("gyi" nil ("$B%.%#(B" . "$B$.$#(B"))
    ("gyo" nil ("$B%.%g(B" . "$B$.$g(B"))
    ("gyu" nil ("$B%.%e(B" . "$B$.$e(B"))
    ;;("h" "" ("$B%*(B" . "$B$*(B"))
    ("ha" nil ("$B%O(B" . "$B$O(B"))
    ("he" nil ("$B%X(B" . "$B$X(B"))
    ("hi" nil ("$B%R(B" . "$B$R(B"))
    ("ho" nil ("$B%[(B" . "$B$[(B"))
    ("hu" nil ("$B%U(B" . "$B$U(B"))
    ("hya" nil ("$B%R%c(B" . "$B$R$c(B"))
    ("hye" nil ("$B%R%'(B" . "$B$R$'(B"))
    ("hyi" nil ("$B%R%#(B" . "$B$R$#(B"))
    ("hyo" nil ("$B%R%g(B" . "$B$R$g(B"))
    ("hyu" nil ("$B%R%e(B" . "$B$R$e(B"))
    ("i" nil ("$B%$(B" . "$B$$(B"))
    ("jj" "j" ("$B%C(B" . "$B$C(B"))
    ("ja" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("je" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("ji" nil ("$B%8(B" . "$B$8(B"))
    ("jo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("ju" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("jya" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("jye" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("jyi" nil ("$B%8%#(B" . "$B$8$#(B"))
    ("jyo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("jyu" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("kk" "k" ("$B%C(B" . "$B$C(B"))
    ("ka" nil ("$B%+(B" . "$B$+(B"))
    ("ke" nil ("$B%1(B" . "$B$1(B"))
    ("ki" nil ("$B%-(B" . "$B$-(B"))
    ("ko" nil ("$B%3(B" . "$B$3(B"))
    ("ku" nil ("$B%/(B" . "$B$/(B"))
    ("kya" nil ("$B%-%c(B" . "$B$-$c(B"))
    ("kye" nil ("$B%-%'(B" . "$B$-$'(B"))
    ("kyi" nil ("$B%-%#(B" . "$B$-$#(B"))
    ("kyo" nil ("$B%-%g(B" . "$B$-$g(B"))
    ("kyu" nil ("$B%-%e(B" . "$B$-$e(B"))
    ("ma" nil ("$B%^(B" . "$B$^(B"))
    ("me" nil ("$B%a(B" . "$B$a(B"))
    ("mi" nil ("$B%_(B" . "$B$_(B"))
    ("mo" nil ("$B%b(B" . "$B$b(B"))
    ("mu" nil ("$B%`(B" . "$B$`(B"))
    ("mya" nil ("$B%_%c(B" . "$B$_$c(B"))
    ("mye" nil ("$B%_%'(B" . "$B$_$'(B"))
    ("myi" nil ("$B%_%#(B" . "$B$_$#(B"))
    ("myo" nil ("$B%_%g(B" . "$B$_$g(B"))
    ("myu" nil ("$B%_%e(B" . "$B$_$e(B"))
    ("n" nil ("$B%s(B" . "$B$s(B"))
    ("n'" nil ("$B%s(B" . "$B$s(B"))
    ("na" nil ("$B%J(B" . "$B$J(B"))
    ("ne" nil ("$B%M(B" . "$B$M(B"))
    ("ni" nil ("$B%K(B" . "$B$K(B"))
    ("nn" nil ("$B%s(B" . "$B$s(B"))
    ("no" nil ("$B%N(B" . "$B$N(B"))
    ("nu" nil ("$B%L(B" . "$B$L(B"))
    ("nya" nil ("$B%K%c(B" . "$B$K$c(B"))
    ("nye" nil ("$B%K%'(B" . "$B$K$'(B"))
    ("nyi" nil ("$B%K%#(B" . "$B$K$#(B"))
    ("nyo" nil ("$B%K%g(B" . "$B$K$g(B"))
    ("nyu" nil ("$B%K%e(B" . "$B$K$e(B"))
    ("o" nil ("$B%*(B" . "$B$*(B"))
    ("pp" "p" ("$B%C(B" . "$B$C(B"))
    ("pa" nil ("$B%Q(B" . "$B$Q(B"))
    ("pe" nil ("$B%Z(B" . "$B$Z(B"))
    ("pi" nil ("$B%T(B" . "$B$T(B"))
    ("po" nil ("$B%](B" . "$B$](B"))
    ("pu" nil ("$B%W(B" . "$B$W(B"))
    ("pya" nil ("$B%T%c(B" . "$B$T$c(B"))
    ("pye" nil ("$B%T%'(B" . "$B$T$'(B"))
    ("pyi" nil ("$B%T%#(B" . "$B$T$#(B"))
    ("pyo" nil ("$B%T%g(B" . "$B$T$g(B"))
    ("pyu" nil ("$B%T%e(B" . "$B$T$e(B"))
    ("rr" "r" ("$B%C(B" . "$B$C(B"))
    ("ra" nil ("$B%i(B" . "$B$i(B"))
    ("re" nil ("$B%l(B" . "$B$l(B"))
    ("ri" nil ("$B%j(B" . "$B$j(B"))
    ("ro" nil ("$B%m(B" . "$B$m(B"))
    ("ru" nil ("$B%k(B" . "$B$k(B"))
    ("rya" nil ("$B%j%c(B" . "$B$j$c(B"))
    ("rye" nil ("$B%j%'(B" . "$B$j$'(B"))
    ("ryi" nil ("$B%j%#(B" . "$B$j$#(B"))
    ("ryo" nil ("$B%j%g(B" . "$B$j$g(B"))
    ("ryu" nil ("$B%j%e(B" . "$B$j$e(B"))
    ("ss" "s" ("$B%C(B" . "$B$C(B"))
    ("sa" nil ("$B%5(B" . "$B$5(B"))
    ("se" nil ("$B%;(B" . "$B$;(B"))
    ("sha" nil ("$B%7%c(B" . "$B$7$c(B"))
    ("she" nil ("$B%7%'(B" . "$B$7$'(B"))
    ("shi" nil ("$B%7(B" . "$B$7(B"))
    ("sho" nil ("$B%7%g(B" . "$B$7$g(B"))
    ("shu" nil ("$B%7%e(B" . "$B$7$e(B"))
    ("si" nil ("$B%7(B" . "$B$7(B"))
    ("so" nil ("$B%=(B" . "$B$=(B"))
    ("su" nil ("$B%9(B" . "$B$9(B"))
    ("sya" nil ("$B%7%c(B" . "$B$7$c(B"))
    ("sye" nil ("$B%7%'(B" . "$B$7$'(B"))
    ("syi" nil ("$B%7%#(B" . "$B$7$#(B"))
    ("syo" nil ("$B%7%g(B" . "$B$7$g(B"))
    ("syu" nil ("$B%7%e(B" . "$B$7$e(B"))
    ("tt" "t" ("$B%C(B" . "$B$C(B"))
    ("ta" nil ("$B%?(B" . "$B$?(B"))
    ("te" nil ("$B%F(B" . "$B$F(B"))
    ("tha" nil ("$B%F%!(B" . "$B$F$!(B"))
    ("the" nil ("$B%F%'(B" . "$B$F$'(B"))
    ("thi" nil ("$B%F%#(B" . "$B$F$#(B"))
    ("tho" nil ("$B%F%g(B" . "$B$F$g(B"))
    ("thu" nil ("$B%F%e(B" . "$B$F$e(B"))
    ("ti" nil ("$B%A(B" . "$B$A(B"))
    ("to" nil ("$B%H(B" . "$B$H(B"))
    ("tsu" nil ("$B%D(B" . "$B$D(B"))
    ("tu" nil ("$B%D(B" . "$B$D(B"))
    ("tya" nil ("$B%A%c(B" . "$B$A$c(B"))
    ("tye" nil ("$B%A%'(B" . "$B$A$'(B"))
    ("tyi" nil ("$B%A%#(B" . "$B$A$#(B"))
    ("tyo" nil ("$B%A%g(B" . "$B$A$g(B"))
    ("tyu" nil ("$B%A%e(B" . "$B$A$e(B"))
    ("u" nil ("$B%&(B" . "$B$&(B"))
    ("vv" "v" ("$B%C(B" . "$B$C(B"))
    ("va" nil ("$B%t%!(B" . "$B$&!+$!(B"))
    ("ve" nil ("$B%t%'(B" . "$B$&!+$'(B"))
    ("vi" nil ("$B%t%#(B" . "$B$&!+$#(B"))
    ("vo" nil ("$B%t%)(B" . "$B$&!+$)(B"))
    ("vu" nil ("$B%t(B" . "$B$&!+(B"))
    ("ww" "w" ("$B%C(B" . "$B$C(B"))
    ("wa" nil ("$B%o(B" . "$B$o(B"))
    ("we" nil ("$B%&%'(B" . "$B$&$'(B"))
    ("wi" nil ("$B%&%#(B" . "$B$&$#(B"))
    ("wo" nil ("$B%r(B" . "$B$r(B"))
    ("wu" nil ("$B%&(B" . "$B$&(B"))
    ("xx" "x" ("$B%C(B" . "$B$C(B"))
    ("xa" nil ("$B%!(B" . "$B$!(B"))
    ("xe" nil ("$B%'(B" . "$B$'(B"))
    ("xi" nil ("$B%#(B" . "$B$#(B"))
    ("xka" nil ("$B%u(B" . "$B$+(B"))
    ("xke" nil ("$B%v(B" . "$B$1(B"))
    ("xo" nil ("$B%)(B" . "$B$)(B"))
    ("xtsu" nil ("$B%C(B" . "$B$C(B"))
    ("xtu" nil ("$B%C(B" . "$B$C(B"))
    ("xu" nil ("$B%%(B" . "$B$%(B"))
    ("xwa" nil ("$B%n(B" . "$B$n(B"))
    ("xwe" nil ("$B%q(B" . "$B$q(B"))
    ("xwi" nil ("$B%p(B" . "$B$p(B"))
    ("xya" nil ("$B%c(B" . "$B$c(B"))
    ("xyo" nil ("$B%g(B" . "$B$g(B"))
    ("xyu" nil ("$B%e(B" . "$B$e(B"))
    ("yy" "y" ("$B%C(B" . "$B$C(B"))
    ("ya" nil ("$B%d(B" . "$B$d(B"))
    ("ye" nil ("$B%$%'(B" . "$B$$$'(B"))
    ("yo" nil ("$B%h(B" . "$B$h(B"))
    ("yu" nil ("$B%f(B" . "$B$f(B"))
    ("zz" "z" ("$B%C(B" . "$B$C(B"))
    ("z," nil "$B!E(B")
    ("z-" nil "$B!A(B")
    ("z." nil "$B!D(B")
    ("z/" nil "$B!&(B")
    ("z[" nil "$B!X(B")
    ("z]" nil "$B!Y(B")
    ("za" nil ("$B%6(B" . "$B$6(B"))
    ("ze" nil ("$B%<(B" . "$B$<(B"))
    ("zh" nil "$B"+(B")
    ("zi" nil ("$B%8(B" . "$B$8(B"))
    ("zj" nil "$B"-(B")
    ("zk" nil "$B",(B")
    ("zl" nil "$B"*(B")
    ("zo" nil ("$B%>(B" . "$B$>(B"))
    ("zu" nil ("$B%:(B" . "$B$:(B"))
    ("zya" nil ("$B%8%c(B" . "$B$8$c(B"))
    ("zye" nil ("$B%8%'(B" . "$B$8$'(B"))
    ("zyi" nil ("$B%8%#(B" . "$B$8$#(B"))
    ("zyo" nil ("$B%8%g(B" . "$B$8$g(B"))
    ("zyu" nil ("$B%8%e(B" . "$B$8$e(B"))
    ("." nil skk-current-kuten)
    ("," nil skk-current-touten)
    ("-" nil "$B!<(B")
    (":" nil "$B!'(B")
    (";" nil "$B!((B")
    ("?" nil "$B!)(B")
    ("[" nil "$B!V(B")
    ("]" nil "$B!W(B")
    ("l" nil skk-latin-mode)
    ("q" nil skk-toggle-kana)
    ("L" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-set-henkan-point-subr)
    ("X" nil skk-purge-from-jisyo)
    ("/" nil skk-abbrev-mode)
    ("$" nil skk-display-code-for-char-at-point)
    ("@" nil skk-today)
    ("\\" nil skk-input-by-code-or-menu)
    (skk-kakutei-key nil skk-kakutei)
    ;; XXX
    ;;("\t" nil skk-insert)
    ;;("," nil skk-previous-candidate)
    ;;("\M-\040" nil skk-start-henkan-with-completion); M-SPC
    ;;("\M-\121" nil skk-backward-and-set-henkan-point); M-Q
    )
  ;; $B%3%s%9%?%s%H$K$7$F$7$^$o$J$$$N$O!"%m!<%^;zF~NO$H$OA4$/JL$N@_Dj$r(B
  ;; $B$9$k?M$b$$$k$+$i$G$9!#(B
  "*$B%-!<F~NO$r$$$+$K=hM}$9$k$+$rI=$9!">uBVA+0\5,B'$N%j%9%H!#(B

$B%j%9%H$N3FMWAG$O!"$=$l$>$l$,0l$D$N5,B'$G$"$j!"2<5-$N7A<0$rK~$?$7$F$$$J$1$l$P(B
$B$J$i$J$$!#(B

 (INPUT-STATE NEXT-STATE OUTPUT)

SKK $B$O(B INPUT-STATE $B$r8!=P$9$k$H!"(BOUTPUT $B$r%P%C%U%!$KA^F~$7!"B3$$$F(B
NEXT-STATE $B$K>uBV$r0\$7$?$&$($G!"F~NOBT$A>uBV$H$J$k!#(B

$BNc$($P!"(B

     (\"a\" nil (\"$B%"(B\" . \"$B$"(B\"))
     (\"ki\" nil (\"$B%-(B\" . \"$B$-(B\"))
     (\"tt\" \"t\" (\"$B%C(B\" . \"$B$C(B\"))
     (\"nn\" nil (\"$B%s(B\" . \"$B$s(B\"))
     (\"n'\" nil (\"$B%s(B\" . \"$B$s(B\"))

$B>e5-$N5,B'$O!"$=$l$>$l!"(B

     a  => $B$"(B
     ki => $B$-(B
     tt => $B$C(Bt
     nn => $B$s(B
     n' => $B$s(B

$B$3$N$h$&$K>uBV$,0\$jJQ$o$k$3$H$r0UL#$9$k!#(B

INPUT-STATE $B$*$h$S(B NEXT-STATE $B$O!"DL>o(B US-ASCII $BJ8;z$+$i$J$kJ8;zNs$rMQ$$$k!#(B
$B$?$@$7!"FCJL$J>l9g$K$O(B INPUT-STATE $B$K$=$l0J30$NJ8;zNs$r;XDj$9$k$3$H$,$"$k!#(B

OUTPUT $B$K$O!"0J2<$N(B 3$B$D$N7A<0$r;XDj$G$-$k!#(B

$BJ8;zNs(B -- $B$+$J%b!<%I!"%+%J%b!<%I$H$b!"$3$l$,A^F~$5$l$k!#(B
$BJ8;zNs$HJ8;zNs$N%;%k(B ($B%I%C%H%Z%"(B)
       -- $B$+$J%b!<%I$K$*$$$F$O(B CDR $B$N!"%+%J%b!<%I$K$*$$$F$O(B CAR $B$NJ8;zNs$,!"(B
          $B$=$l$>$lA^F~$5$l$k!#(B
$B4X?tL>%7%s%\%k(B
       -- $B4X?t$r<B9T$9$k!#$b$7$=$N4X?t$NJV$jCM$,J8;zNs$J$i$P!"$=$NJ8;zNs$r(B
          $BA^F~$9$k!#(B

$BF1MM$N5,B'$rI=$9JQ?t$K(B `skk-rom-kana-rule-list' $B$,$"$k!#(BSKK $B$ON>J}$N5,B'$rMx(B
$BMQ$9$k$,!"(B `skk-rom-kana-rule-list' $B$NJ}$,M%@h$5$l$k!#=>$C$F%f!<%6$,FH<+$N5,(B
$BB'$r@_Dj$7$?$$>l9g$K$O!"(B`skk-rom-kana-rule-list' $B$NJ}$r;H$&$N$,$h$$!#(B"
  :type '(repeat
	  (list :tag "Rule"
		(string :tag "1 Input State (string)")
		(choice :tag "2 Next State (choice)"
			string
			(const nil))
		(choice :tag "3 Output (choice)"
			(symbol :tag "Function")
			string
			(cons (string :tag "3-1 Katakana (string)")
			      (string :tag "3-2 Hiragana (string)")))))
  :group 'skk-keybinds)

(defcustom skk-rom-kana-rule-list
  '(;; $B%f!<%6!<$N9%$_$G@_Dj$,J,$l$=$&$JMWAG$O!"(B
    ;; skk-rom-kana-base-rule-list $B$+$i$3$A$i$X0\$7$^$7$g$&(B...$B!#(B
    ("hh" "h" ("$B%C(B" . "$B$C(B"))
    ;; when you may want to insert $B!V$,$s$^!W(Bby "gamma"...
    ("mm" "m" ("$B%s(B" . "$B$s(B")))
  "*$B>uBVA+0\5,B'$N%j%9%H$G!"%f!<%6$NDI2C@_DjMQ$NJQ?t!#(B

$B$3$NJQ?t$O!"(B`skk-rom-kana-base-rule-list' $B$HF1MM$N=q<0$rK~$?$9I,MW$,$"$k!#(B

SKK $B$O5/F0;~$K$3$N(B 2 $BJQ?t$rJT=8$7$F(B `skk-rule-tree' $B$r:n@.$9$k$,!"(B
`skk-rom-kana-rule-list' $B$N5,B'$O(B `skk-rom-kana-base-rule-list' $B$N5,B'$h$j$b(B
$BM%@h$5$l$k!#(B

$B%j%9%H$N3FMWAG$O!"$=$l$>$l$,0l$D$N5,B'$G$"$j!"2<5-$N7A<0$rK~$?$7$F$$$J$1$l$P(B
$B$J$i$J$$!#(B

 (INPUT-STATE NEXT-STATE OUTPUT)

SKK $B$O(B INPUT-STATE $B$r8!=P$9$k$H!"(BOUTPUT $B$r%P%C%U%!$KA^F~$7!"B3$$$F(B
NEXT-STATE $B$K>uBV$r0\$7$?$&$($G!"F~NOBT$A>uBV$H$J$k!#(B

$B>\$7$/$O!"(B`skk-rom-kana-base-rule-list' $B$N@bL@$r;2>H$N$3$H!#(B

$B%f!<%6$O!"DI2C$7$?$$5,B'$r!"Nc$($P(B

    (setq skk-rom-kana-rule-list
      '(
	(\"hh\" \"h\" (\"$B%C(B\" . \"$B$C(B\"))
	(\"@\" nil \"$B!w(B\")
	...
	))

$B>e5-$N$h$&$K(B `.emacs' $B$^$?$O(B `skk-init-file' $B$K$F@_Dj$9$k$3$H$,$G$-$k!#(B

$B$3$NJQ?t$OI8=`$G$O!"(B

    (\"hh\" \"h\" (\"$B%C(B\" . \"$B$C(B\"))

$B>e5-$N@_Dj$,$5$l$F$$$k!#$3$N5,B'$K$h$k$H!"(B

    ohhonn => $B$*$C$[$s(B
    ohhira => $B$*$C$R$i(B

$B$N$h$&$KA^F~$5$l$k!#$b$7$3$l$r(B

    ohhonn  => $B$*$*$[$s(B
    ohhira  => $B$*$*$R$i(B

$B$N$h$&$KJQ99$7$?$1$l$P!"(B

    (\"hh\" \"h\" (\"$B%C(B\" . \"$B$C(B\"))

$B$3$N@_Dj$r:o=|$9$k!#(B

$B$^$?!"(B`@' $B$G(B `skk-today' ($BEvF|$NF|IU$NF~NO(B) $B$r5/F0$9$kBe$j$K(B `$B!w(B' $B$rF~(B
$BNO$7$?$$>l9g$O!"(B`skk-rom-kana-rule-list' $B$K(B

    (\"@\" nil \"$B!w(B\")

$B$H$$$&MWAG$r2C$($k!#(B

$B$b$7!"(BSKK $B$r5/F0$7$?8e$G(B `skk-rom-kana-rule-list' $B$NJQ99$r9T$C$?>l9g!"$=$N@_(B
$BDj$rH?1G$5$;$k$K$O(B \\[skk-restart] $B$r<B9T$9$kI,MW$,$"$k!#(B"
  :type '(repeat
	  (list :tag "Rule"
		(string :tag "1 Input State (string)")
		(choice :tag "2 Next State (choice)"
			string
			(const nil))
		(choice :tag "3 Output (choice)"
			(symbol :tag "Function")
			string
			(cons (string :tag "3-1 Katakana (string)")
			      (string :tag "3-2 Hiragana (string)")))))
  :group 'skk-keybinds)

(defcustom skk-kana-input-search-function
  (function
   (lambda ()
     (save-match-data
       (and (string-match "^h\\([bcdfghjklmnpqrstvwxz]\\)$" skk-prefix)
	    (member (char-to-string (preceding-char)) '("$B$*(B" "$B%*(B"))
	    (cons '("$B%*(B" . "$B$*(B") (match-string 1 skk-prefix))))))
  "*$B%k!<%k%j%9%H$NCf$K5-$;$J$$JQ49%k!<%k$r=hM}$9$k4X?t!#(B
`skk-rom-kana-base-rule-list' $B$H(B `skk-rom-kana-rule-list' $B$NMWAG$rA4$F8!:w(B
$B$7$?8e$K%3!<%k$5$l$k!#0z?t$O$J$$!#(B

 ($B8=:_$NF~NO$KBP$9$k=PNO(B . \"$BB3$/(B unfixed prefix\")

$B$H$$$&%;%k$rJV$9!#=PNO$N<oN`$K$D$$$F$O(B `skk-rom-kana-base-rule-list' $B$r(B
$B;2>H$N$3$H!#(B

$B%G%#%U%)%k%H$G$O!"(B\"$B$*(B\" $B$N8e$N(B \"h\" + $B;R2;$NF~NO$r(B \"$B$*$*(B\" + $BB3$/;R(B
$B2;=hM}MQ$N(B unfixed prefix $B$KJQ49$7$F$$$k!#(B"
  :type 'function
  :group 'skk-hooks-and-functions)

(defcustom skk-okuri-char-alist nil
  "*$BAw$j2>L>(B prefix $B$rJQ49$9$k%k!<%k$r5-=R$9$kO"A[%j%9%H!#(B
car $B$K!V<B:]$N%-!<F~NO$K$h$k$+$J(B prefix $BJ8;zNs!W!"(Bcdr $B$K!V(BSKK $B$N<-=q$,M=(B
$BA[$7$F$$$k$+$J(B prefix $BJ8;zNs!W$r;}$D(B cons cell $B$N%j%9%H!#(B

$B$3$N5,B'$,;H$o$l$k$N$O!"(B`skk-process-okuri-early' $B$,Hs(B nil $B$N>l9g$N$_$G$"$k!#(B

$BNc$($P!"$+9T$NAw$j2>L>F~NO$K(B \"c\" $B$N(B prefix $B$r;H$&$N$G$"$l$P!"(B

  (setq skk-okuri-char-alist '((\"c\" . \"k\")))

$B$N$h$&$K=q$/!#(B"
  :type '(repeat (cons string string))
  :group 'skk-okurigana)

(defcustom skk-downcase-alist nil
  "*$BJQ49%-!<(B ($BBgJ8;z%m!<%^;z(B) $B$N>.J8;z$X$NJQ495,B'$rI=$o$9O"A[%j%9%H!#(B
$BJQ49%-!<$NF~NO$r3+;O$9$k:]!"(BSKK $B$G$OBgJ8;z$GF~NO$r9T$&$N$G!"(B
`skk-set-henkan-point' $B$NCf$G$3$l$r>.J8;z$KJQ49$9$k:n6H$r9T$&!#$3$NO"A[(B
$B%j%9%H$KBgJ8;z(B -> $B>.J8;z$NJQ49%k!<%k$r=q$$$F$*$/$3$H$G!"%-!<F~NO$N%+%9(B
$B%?%^%$%:$r9T$&$3$H$,$G$-$k!#$3$NO"A[%j%9%H$,6u%j%9%H$N>l9g$O!"C1$K(B
downcase $B$5$l$k!#(B"
  :type '(repeat (cons character character))
  :group 'skk-keybinds)

(defcustom skk-jisx0208-latin-vector
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "$B!!(B"  "$B!*(B" "$B!I(B" "$B!t(B" "$B!p(B" "$B!s(B" "$B!u(B" "$B!G(B"
   "$B!J(B" "$B!K(B" "$B!v(B" "$B!\(B" "$B!$(B" "$B!](B" "$B!%(B" "$B!?(B"
   "$B#0(B" "$B#1(B" "$B#2(B" "$B#3(B" "$B#4(B" "$B#5(B" "$B#6(B" "$B#7(B"
   "$B#8(B" "$B#9(B" "$B!'(B" "$B!((B" "$B!c(B" "$B!a(B" "$B!d(B" "$B!)(B"
   "$B!w(B" "$B#A(B" "$B#B(B" "$B#C(B" "$B#D(B" "$B#E(B" "$B#F(B" "$B#G(B"
   "$B#H(B" "$B#I(B" "$B#J(B" "$B#K(B" "$B#L(B" "$B#M(B" "$B#N(B" "$B#O(B"
   "$B#P(B" "$B#Q(B" "$B#R(B" "$B#S(B" "$B#T(B" "$B#U(B" "$B#V(B" "$B#W(B"
   "$B#X(B" "$B#Y(B" "$B#Z(B" "$B!N(B" "$B!@(B" "$B!O(B" "$B!0(B" "$B!2(B"
   "$B!F(B" "$B#a(B" "$B#b(B" "$B#c(B" "$B#d(B" "$B#e(B" "$B#f(B" "$B#g(B"
   "$B#h(B" "$B#i(B" "$B#j(B" "$B#k(B" "$B#l(B" "$B#m(B" "$B#n(B" "$B#o(B"
   "$B#p(B" "$B#q(B" "$B#r(B" "$B#s(B" "$B#t(B" "$B#u(B" "$B#v(B" "$B#w(B"
   "$B#x(B" "$B#y(B" "$B#z(B" "$B!P(B" "$B!C(B" "$B!Q(B" "$B!A(B" nil]
  "*`skk-jisx0208-latin-insert' $B$G;2>H$5$l$kJ8;z%F!<%V%k!#(B
$B%-!<$KBP1~$9$k0LCV$KJ8;zNs$,$"$l$P!"A41Q%b!<%I$G3:Ev$N%-!<$r2!$9$3$H$G!"BP1~$9(B
$B$kJ8;z$,A^F~$5$l$k!#(B
$BNc$($P!"%9%Z!<%9%-!<$KBP1~$7$F!"H>3Q%9%Z!<%9$rA^F~$5$;$k$h$&$KJQ99$7$?$1$l$P!"(B
skk.el $B$N%m!<%I8e(B ($B$b$7$/$O(B `skk-load-hook' $B$rMxMQ$7$F(B)$B!"(B

     (aset skk-jisx0208-latin-vector 32 \" \")

$B$H$9$k$+!"$b$7$/$O!"(B`skk-jisx0208-latin-vector' $B$N(B 32 $BHVL\(B (0 $BHV$+$i?t$($F(B)
 $B$NCM$r(B \" \"$B$H$9$k$h$&$J(B `skk-jisx0208-latin-vector' $B$rD>@\=q$-!"(Bsetq $B$G(B
$BBeF~$9$k!#(B32 $B$O!"(B?  ($BH>3Q%9%Z!<%9$N(B char type) $B$rI>2A$7$?$H$-$NCM!#(B"
  :type 'sexp
  :group 'skk-keybinds)

(defcustom skk-use-face (or window-system
			    (fboundp 'selected-frame)
			    ; XEmacs does not have this.
			    (fboundp 'frame-face-alist)
			    (> emacs-major-version 20))
  "*Non-nil $B$G$"$l$P!"(BEmacs $B$N(B face $B$N5!G=$r;HMQ$7$FJQ49I=<($r9T$&!#(B"
  :type 'boolean
  :group 'skk-decoration)

;; should use defface?  however, can I use defface for highlight?
(defcustom skk-henkan-face 'skk-henkan-face-default
  "*$BJQ498uJd$N(B face $BB0@-!#(B`skk-use-face' $B$,(B non-nil $B$N$H$-$N$_M-8z!#(B
Emacs $BI8=`%U%'%$%9$N(B default, modeline, region, secondary-selection,
highlight, underline, bold, italic, bold-italic $B$NB>!"?7$?$K(B face $B$r:n(B
$B$j;XDj$9$k$3$H$b2DG=!#(B
$B?7$?$J(B face $B$r:n$j;XDj$9$k$K$O(B `skk-make-face' $B$rMxMQ$7$F!"(B

      (skk-make-face 'DimGray/PeachPuff1)
      (setq skk-henkan-face 'DimGray/PeachPuff1)

$B$N$h$&$K$9$k$N$,<j7Z!#(Bforeground $B$H(B background $B$N?';XDj$@$1$G$J$$6E$C$?(B face
$B$r:n$k>l9g$O!"(B`skk-make-face' $B$G$OBP1~$G$-$J$$$N$G!"(BEmacs $B$N(B hilit19.el $B$N(B
`hilit-lookup-face-create' $B$J$I$rMxMQ$9$k!#?'$rIU$1$k>l9g$NG[?'$O!"(Bcanna.el $B$N(B
`canna:attribute-alist' $B$,NI$$Nc$+$b$7$l$J$$!#(B"
  :type 'face
  :group 'skk-decoration)

(defface skk-henkan-face-default
  '((((class color) (type tty))
     (:foreground "black" :background "green"))
    (((class color) (background light))
     (:foreground "black" :background "darkseagreen2"))
    (((class color) (background dark))
     (:foreground "white" :background "darkolivegreen"))
    (((class grayscale)) (:underline t)))
  "*$BI8=`$NJQ498uJd$N(B face $BB0@-!#(B"
  :group 'skk-decoration)

(when (and skk-use-face
	   (not frame-background-mode)
	   (not (face-background 'skk-henkan-face-default)))
  (set-face-foreground 'skk-henkan-face-default "black")
  (set-face-background 'skk-henkan-face-default "darkseagreen2"))

;;; SKK-AUTO.EL related.
(defcustom skk-okuri-search-function 'skk-okuri-search-subr-original
  "*skk-okuri-search $B$G;HMQ$9$k4X?t!#(B"
  :type 'function
  :group 'skk-hooks-and-functions
  :group 'skk-auto)

(defcustom skk-auto-load-hook nil
  "*skk-auto.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-hooks-and-functions
  :group 'skk-auto)

;;; SKK-COMP.EL related.
(defcustom skk-dabbrev-like-completion nil
  "*Non-nil $B$G$"$l$P!":G8e$KJd40$5$l$?8l$K$D$$$F99$KJd40$,9T$o$l$k!#(B
$BNc$($P!"(B

  \"$B$5(B\" (,) -> \"$B$5$H$&(B\" (,) -> \"$B$5$H$&$;$s$;$$(B\"

nil $B$G$"$l$P!"@hF,$NJ8;z$r6&DL$K$9$kJ8;zNs$K$D$$$FJd40$,9T$o$l$k!#(B
$BNc$($P!"(B

  \"$B$5(B\" (,) -> \"$B$5$H$&(B\" (,) -> \"$B$5$$$H$&(B\" (,) -> \"$B$5$/$i(B\""
  :type 'boolean
  :group 'skk-comp)

(defcustom skk-comp-load-hook nil
  "*skk-comp.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-hooks-and-functions
  :group 'skk-comp)

(defcustom skk-kakutei-history-limit 100
  "$BJQ?t(B `skk-kakutei-history' $B$NCM(B ($BO"A[%j%9%H(B) $B$ND9$5$N>e8B!#(B"
  :type 'integer
  :group 'skk-misc
  :group 'skk-comp)

(defcustom skk-use-color-cursor (and window-system
				     (fboundp 'x-display-color-p)
				     (x-display-color-p))
  "*Non-nil $B$G$"$l$P!"(BSKK $B%b!<%I$NF~NO%b!<%I$K1~$8$F%+!<%=%k$K?'$rIU$1$k!#(B"
  :type 'boolean
  :group 'skk-decoration
  :group 'skk-cursor)

(defcustom skk-auto-insert-paren nil
  "*Non-nil $B$G$"$l$P!"3g8L$HJD3g8L$r$^$H$a$FA^F~$9$k!#(B
$BNc$($P!"(B\"$B!V(B\" $B$rF~NO$7$?$H$-$K(B \"$B!W(B\" $B$r<+F0E*$KA^F~$7!"N>$+$.$+$C$3$N4V$K(B
$B%+!<%=%k$r0\F0$9$k!#(B
$BA^F~$9$kJ8;zNs$O!"(B`skk-auto-paren-string-alist' $B$G;XDj$9$k!#(B"
  :type 'boolean
  :group 'skk-keybinds)

(defcustom skk-auto-paren-string-alist
  '(("$B!V(B" . "$B!W(B") ("$B!X(B" . "$B!Y(B") ("(" . ")") ("$B!J(B" . "$B!K(B")
    ("{" . "}")("$B!P(B" . "$B!Q(B") ("$B!R(B" . "$B!S(B") ("$B!T(B" . "$B!U(B")
    ("[" . "]") ("$B!N(B" . "$B!O(B") ("$B!L(B" . "$B!M(B") ("$B!Z(B" . "$B![(B")
    ("\"" . "\"")("$B!H(B" . "$B!I(B") ("`" . "'")
    ;;("<" . ">") ;; skk-special-midashi-char-list $B$NCf$K$"$kJ8;z!#(B
  )
  "*$B<+F0E*$KBP$K$J$kJ8;zNs$rF~NO$9$k$?$a$NO"A[%j%9%H!#(B
`skk-auto-insert-paren' $B$,(B non-nil $B$N>l9g!"(Bcar $B$NJ8;zNs$,A^F~$5$l$?$H$-(B
$B$K(B cdr $B$NJ8;zNs$r<+F0E*$KA^F~$5$l!"%+!<%=%k$O$=$N(B 2 $B$D$NJ8;zNs$N4V$K0\(B
$BF0$9$k!#(B
`skk-special-midashi-char-list' $B$NMWAG$K$J$C$F$$$kJ8;z$O!"(B
`skk-auto-paren-string-alist' $B$K4^$a$F$b:o=|$5$l$k!#(B "
  :type '(repeat (cons string string))
  :group 'skk-keybinds)

(defcustom skk-japanese-message-and-error nil
  "*Non-nil $B$G$"$l$P!"(BSKK $B$N%a%C%;!<%8$H%(%i!<$rF|K\8l$GI=<($9$k!#(B
nil $B$G$"$l$P!"1Q8l$GI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-decoration)

(defcustom skk-set-henkan-point-key
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?M ?N ?O ?P ?R ?S ?T ?U ?V ?W ?Y ?Z)
  "*$BJQ49$N3+;OCOE@$r7h$a$k%-!<$N%j%9%H!#(B"
  :type '(repeat character)
  :group 'skk-keybinds)

(defcustom skk-emacs-id-file (convert-standard-filename "~/.skk-emacs-id") "\
*`skk-jisyo-file'$B$K:G6a%"%/%;%9$7$?(B SKK $B$N(B `skk-emacs-id' $B$rJ]B8$9$k%U%!%$%k!#(B"
  :type 'file
  :group 'skk-filenames)

(defcustom skk-share-private-jisyo nil "\
*Non-nil $B$G$"$l$P!"J#?t$N(B SKK $B$K$h$k8D?M<-=q$N6&M-$r9MN8$7$F<-=q$r99?7$9$k!#(B"
  :type 'boolean
  :group 'skk-dictionary)

(defcustom skk-jisyo-save-count 50
  "*$B?tCM$G$"$l$P!"$=$N2s?t<-=q$,99?7$5$l$?$H$-$K<-=q$r<+F0E*$K%;!<%V$9$k!#(B
nil $B$G$"$l$P!"<-=q$N%*!<%H%;!<%V$r9T$o$J$$!#(B"
  :type '(choice integer (const nil))
  :group 'skk-dictionary)

(defcustom skk-byte-compile-init-file nil
  "*Non-nil $B$G$"$l$P!"(Bskk-mode $B5/F0;~$K(B skk-init-file $B$r%P%$%H%3%s%Q%$%k$9$k!#(B
$B@53N$K8@$&$H!"(B

  (1)skk-init-file $B$r%P%$%H%3%s%Q%$%k$7$?%U%!%$%k$,$J$$$+!"(B
  (2)skk-init-file $B$H$=$N%P%$%H%3%s%Q%$%k:Q%U%!%$%k$rHf3S$7$F!"A0<T$NJ}$,?7$7(B
     $B$$$H$-(B

$B$K(B skk-init-file $B$r%P%$%H%3%s%Q%$%k$9$k!#(B
nil $B$G$"$l$P!"(Bskk-init-file $B$H$=$N%P%$%H%3%s%Q%$%k:Q$_%U%!%$%k$rHf3S$7$F(B
skk-init-file $B$NJ}$,?7$7$$$H$-$O!"$=$N%P%$%H%3%s%Q%$%k:Q%U%!%$%k$r>C$9!#(B"
  :type 'boolean
  :group 'skk-misc)

(defcustom skk-count-private-jisyo-candidates-exactly nil
  "*Non-nil $B$G$"$l$P!"(BEmacs $B$r=*N;$9$k$H$-$K@53N$K8D?M<-=q$N8uJd?t$r?t$($k!#(B
nil $B$G$"$l$P!"(B1 $B9T$KJ#?t$N8uJd$,$"$C$F$b(B 1 $B8uJd$H$7$F?t$($k!#(B
$B7W;;7k2L$O!"(B`skk-record-file' $B$KJ]B8$5$l$k!#(B"
  :type 'boolean
  :group 'skk-dictionary)

(defcustom skk-compare-jisyo-size-when-saving t
  "*Non-nil $B$G$"$l$P!"(B`skk-jisyo' $B$N%;!<%V;~$K%U%!%$%k%5%$%:$N%A%'%C%/$r9T$&!#(B
$BA02s%;!<%V$7$?(B `skk-jisyo' $B$H:#2s%;!<%V$7$h$&$H$9$k<-=q$H$N%5%$%:Hf3S$r9T$$!"(B
$B8e<T$NJ}$,Bg$-$$$H$-$K%f!<%6!<$K%;!<%V$rB3$1$k$+$I$&$+$N3NG'$r5a$a$k!#(B"
  :type 'boolean
  :group 'skk-dictionary)

(defcustom skk-auto-start-henkan t
  "*$BC18l$dJ8@a$N6h@Z$j$r<($9J8;z$NBG80$K$h$j<+F0E*$KJQ49$r3+;O$9$k!#(B
`skk-auto-start-henkan-keyword-list' $B$K$h$jC18l$dJ8@a$N6h@Z$j$r<($9J8;z$r(B
$B;XDj$9$k!#(B"
  :type 'boolean
  :group 'skk-keybinds)

(defcustom skk-auto-start-henkan-keyword-list
  '("$B$r(B" "$B!"(B" "$B!#(B" "$B!%(B" "$B!$(B" "$B!)(B" "$B!W(B" "$B!*(B" "$B!((B" "$B!'(B" ")" ";" ":"
    "$B!K(B" "$B!I(B" "$B![(B" "$B!Y(B" "$B!U(B" "$B!S(B" "$B!Q(B" "$B!O(B" "$B!M(B" "}" "]" "?" "."
    "," "!")
  ;; $B$"$^$j%-!<%o!<%I$,B?$/$J$k$H!"DL>o$NJQ49$r:$Fq$K$9$k!)(B
  "*$B<+F0JQ49$r3+;O$9$k%-!<%o!<%I!#(B
`skk-auto-start-henkan' $B$,(B non-nil $B$N$H$-!"$3$N%j%9%H$NMWAG$NJ8;z$rA^F~(B
$B$9$k$H!"(BSPC $B$r2!$9$3$H$J$/<+F0E*$KJQ49$r3+;O$9$k!#(B"
  :type '(repeat string)
  :group 'skk-keybinds)

(defcustom skk-search-excluding-word-pattern-function nil
  "*$B8D?M<-=q$K<h$j9~$^$J$$J8;zNs$N%Q%?!<%s$r8!:w$9$k4X?t$r;XDj$9$k!#(B
$B3NDj$7$?J8;zNs$r0z?t$KEO$7$F(B `funcall' $B$5$l$k!#(B

SKK $B$G$OJQ49!"3NDj$r9T$C$?J8;zNs$OA4$F8D?M<-=q$K<h$j9~$^$l$k$,!"$3$N(B
$BJQ?t$G;XDj$5$l$?4X?t$,(B non-nil $B$rJV$9$H$=$NJ8;zNs$O8D?M<-=q$K<h$j9~$^(B
$B$l$J$$!#(B

$BNc$($P!"$3$NJQ?t$K2<5-$N$h$&$J;XDj$9$k$H!"JQ49$K$h$j(B (SKK abbrev mode
$B$G$NJQ49$r=|$/(B) $B%+%?%+%J$N$_$+$i$J$kJ8;zNs$rF@$F3NDj$7$F$b!"$=$l$r8D?M(B
$B<-=q$K<h$j9~$^$J$$!#(B

  (setq skk-search-excluding-word-pattern-function
        (function
         (lambda (kakutei-word)
         ;; $B$3$N4X?t$,(B t $B$rJV$7$?$H$-$O!"$=$NJ8;zNs$O8D?M<-=q$K<h$j9~$^$l$J$$!#(B
           (save-match-data
             (and
              ;; $BAw$j$J$7JQ49$G!"(B
              (not skk-okuri-char)
              ;; $B3NDj8l$,%+%?%+%J$N$_$+$i9=@.$5$l$F$$$F!"(B
              (string-match \"^[$B!<%!(B-$B%s(B]+$\" kakutei-word)
              ;; SKK abbrev mode $B0J30$G$NJQ49$+!"(B
              (or (not skk-abbrev-mode)
                ;; $B8+=P$78l$,%+%?%+%J!"$R$i$,$J0J30$N$H$-!#(B
                ;; ($B8e$G"&%^!<%/$rIU$1$?$H$-$O!"8+=P$78l$,1QJ8;z$G$b!"(B
                ;; skk-abbrev-mode$B$,(B t $B$K$J$C$F$$$J$$(B)$B!#(B
                  (not (string-match \"^[^$B!<%!(B-$B%s$!(B-$B$s(B]+$\"
				      skk-henkan-key))))))))

$B%+%?%+%J$rJQ49$K$h$j5a$a$?$$$,!"8D?M<-=q$K$O%+%?%+%J$N$_$N8uJd$r<h$j9~$_$?(B
$B$/$J$$!"$J$I!"8D?M<-=q$,I,MW0J>e$KKD$l$k$N$rM^$($kL\E*$K;HMQ$G$-$k!#(B

$B$J$*!"8D?M<-=q$K<h$j9~$^$J$$8+=P$78l$K$D$$$F$OJd40$,8z$+$J$$$N$GCm0U$9$k$3$H!#(B"
  :type 'function
  :group 'skk-hooks-and-functions)

(defcustom skk-update-jisyo-function 'skk-update-jisyo-original
  "*skk-update-jisyo $B$G;HMQ$9$k4X?t!#(B"
  :type 'function
  :group 'skk-hooks-and-functions)

(defcustom skk-save-jisyo-function 'skk-save-jisyo-original
  "*skk-save-jisyo $B$G;HMQ$9$k4X?t!#(B"
  :type 'function
  :group 'skk-hooks-and-functions)

(defcustom skk-count-jisyo-candidates-function
  'skk-count-jisyo-candidates-original
  "*skk-count-jisyo-candidates $B$G;HMQ$9$k4X?t!#(B"
  :type 'function
  :group 'skk-hooks-and-functions)

(defcustom skk-public-jisyo-to-be-searched-function
  'skk-public-jisyo-to-be-searched-original
  "*skk-public-jisyo-has-entry-p $B$G;HMQ$9$k4X?t!#(B"
  :type 'function
  :group 'skk-hooks-and-functions)

(defcustom skk-use-look nil
  "*Non-nil $B$G$"$l$P!"(BUNIX look $B%3%^%s%I$rMxMQ$7$?Jd40!&JQ49$r9T$&!#(B
SKK abbrev $B%b!<%I$GJd40$r9T$&$H!"8D?M<-=q$r8!:w$7?T$7$?8e$G!"(BUNIX look $B%3%^%s(B
$B%I$K$h$k1QC18lJd40$r9T$&!#Nc$($P!"(B

  $B"&(Babstr (TAB)
  ---> $B"&(Babstract

SKK abbrev $B%b!<%I$G!"!V1QJ8;z(B + $B%"%9%?%j%9%/!W$K$FJQ49$r9T$&$H!"(Blook $B%3%^%s%I(B
$B$K$h$k$"$$$^$$8!:w$r9T$&$3$H$,$G$-$k!#Nc$($P!"(B

 $B"&(Babstra* (SPC)
  ---> $B"'(Babstract

$B$3$N>uBV$G3NDj$9$k$H!"(B`abstra*' $B$r8+=P$78l!"(B`abstract' $B$r8uJd$H$9$k%(%s%H%j(B
$B$,8D?M<-=q$KDI2C$5$l$k!#(B`skk-search-excluding-word-pattern-function' $B$K$h(B
$B$j!"3NDj$7$F$b$3$N$h$&$J%(%s%H%j$rDI2C$7$J$$$h$&$K@_Dj$9$k$3$H$,$G$-$k!#(B"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-henkan-overlay-priority 600
  "*$BJQ49$7$?8uJd$K=E$M$k(B overlay $B$N(B priority$B!#(B
$BNc$($P!"(BViper $B$G(B R $B%3%^%s%I$K$h$j(B replace $B$r9T$&$H$-$K!"(B
`viper-replace-overlay' $B$H$$$&(B priority 400 $B$N(B overlay $B$r=E$M$i$l$k$,!"(B
`skk-henkan-overlay-priority' $B$N%G%#%U%)%k%HCM$O$3$N(B overlay $B$h$j(B
priority $B$,9b$$$N$G!"M%@h$7$FI=<($5$l$k!#(B"
  :type 'integer
  :group 'skk-decoration)

(defcustom skk-kuten-touten-alist '((jp . ("$B!#(B" . "$B!"(B")) (en . ("$B!%(B" . "$B!$(B")))
  "*$B6gE@$HFIE@$NO"A[%j%9%H!#(B
$B3FMWAG$N7A<0$O!"(B

   ($B%7%s%\%k(B . ($B6gE@$rI=$o$9J8;zNs(B . $BFIE@$rI=$o$9J8;zNs(B))

$B$H$$$&(B cons cell$B!#%7%s%\%k$NItJ,$O!"(B`jp' $B$b$7$/$O(B `en' $B$H$7!"(B
`skk-toggle-kutouten' $B$O$3$l$r%H%0%k$G@Z$j49$($k!#(B
$B%G%#%U%)%k%H$N6gFIE@$N%?%$%W$O!"(B`skk-kutouten-type' $B$G;XDj$9$k!#(B"
  :type '(repeat (cons (choice (const jp) (const en))
		       (cons string string)))
  :group 'skk-keybinds)

(defcustom skk-kutouten-type 'jp
  "*$BI8=`$N6gFIE@$N%?%$%W!#(B`jp' $B$b$7$/$O(B `en' $B$H$$$&%7%s%\%k!#(B
$B%P%C%U%!%m!<%+%kCM!#(B"
  :type '(choice (const jp) (const en))
  :group 'skk-keybinds)
(make-variable-buffer-local 'skk-kutouten-type)

(defcustom skk-read-from-minibuffer-function nil "\
*$BC18lEPO?%b!<%I$G(B `read-from-minibuffer' $B$N(B INITIAL-CONTENTS $B$rDs6!$9$k4X?t!#(B
$B$3$N(B function $B$OJ8;zNs$rJV$5$J$1$l$P$J$i$J$$!#(B
$BNc$($P!"(Bskk-henkan-key $B$r$=$N$^$^(B initial-contents $B$H$7$FMxMQ$7$?$$$H$-$O!"(B

  (setq skk-read-from-minibuffer-function
        (function (lambda () skk-henkan-key)))

$B$H;XDj$9$k!#(B"
  :type 'function
  :group 'skk-hooks-and-functions)

(defcustom skk-use-jisx0201-input-method nil "\
*Non-nil $B$J$i(B $BH>3Q%+%J$H(B Japanese Roman $B$NF~NO5!G=$,MxMQ2DG=$K$J$k!#(B"
  :type 'boolean
  :group 'skk-jisx0201)

(defcustom skk-use-kana-keyboard nil "\
*Non-nil $B$J$i2>L>F~NOMQ$N@_Dj$r%m!<%I$9$k!#(B
SKK $B;HMQCf$K$3$NJQ?t$NCM$r@Z$jBX$($k$3$H$G(B  $B%m!<%^;zF~NO(B $B"+"*(B $B2>L>F~NO(B $B$N(B
$B@Z$jBX$($,$G$-$k!#(B"
  :type 'boolean
  :group 'skk-keybinds)

(defcustom skk-undo-kakutei-word-only nil
  "*Non-nil $B$G$"$l$P(B $B"&%b!<%I$H"'%b!<%I;~$N%"%s%I%%>pJs$r5-O?$7$J$$!#(B"
  :type 'boolean
  :group 'skk-misc)

(defvar skk-latin-mode-map nil
  "*ASCII $B%b!<%I$N%-!<%^%C%W!#(B")
(defvar skk-j-mode-map nil
  "*$B$+$J%b!<%I$N%-!<%^%C%W!#(B")
(defvar skk-jisx0208-latin-mode-map nil
  "*$BA43Q%b!<%I$N%-!<%^%C%W!#(B")
(defvar skk-abbrev-mode-map nil
  "*SKK abbrev $B%b!<%I$N%-!<%^%C%W!#(B")

(defvar skk-menu-items
  ;; SKK $B%a%K%e!<$NDj5A!#(B
  '("SKK"
    ("Convert Region and Echo"
     ("Gyakubiki"
      ["to Hiragana" skk-gyakubiki-message skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
	(function
	 (lambda (start end)
	   (interactive "r")
	   (skk-gyakubiki-message start end 'all-candidates))))
       skk-use-kakasi]
      ["to Katakana" skk-gyakubiki-katakana-message skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
	(function
	 (lambda (start end)
	   (interactive "r")
	   (skk-gyakubiki-katakana-message
	    start end 'all-candidates))))
       skk-use-kakasi])
     ("Hurigana"
      ["to Hiragana" skk-hurigana-message skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
	(function
	 (lambda (start end)
	   (interactive "r")
	   (skk-hurigana-message start end 'all-candidates))))
       skk-use-kakasi]
      ["to Katakana" skk-hurigana-katakana-message skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
	(function
	 (lambda (start end)
	   (interactive "r")
	   (skk-hurigana-katakana-message
	    start end 'all-candidates))))
       skk-use-kakasi]))
    ("Convert Region and Replace"
     ["Ascii" skk-latin-region skk-use-kakasi]
     ("Gyakubiki"
      ["to Hiragana" skk-gyakubiki-region skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
	(function
	 (lambda (start end)
	   (interactive "r")
	   (skk-gyakubiki-region start end 'all-candidates))))
       skk-use-kakasi]
      ["to Katakana" skk-gyakubiki-katakana-region skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
	(function
	 (lambda (start end)
	   (interactive "r")
	   (skk-gyakubiki-katakana-region
	    start end 'all-candidates))))
       skk-use-kakasi])
     ["Hiragana" skk-hiragana-region skk-use-kakasi]
     ("Hurigana"
      ["to Hiragana" skk-hurigana-region skk-use-kakasi]
      ["to Hiragana, All Candidates"
       (call-interactively
	(function
	 (lambda (start end)
	   (interactive "r")
	   (skk-hurigana-region start end 'all-candidates))))
       skk-use-kakasi]
      ["to Katakana" skk-hurigana-katakana-region skk-use-kakasi]
      ["to Katakana, All Candidates"
       (call-interactively
	(function
	 (lambda (start end) (interactive "r")
	   (skk-hurigana-katakana-region
	    start end 'all-candidates))))
       skk-use-kakasi])
     ["Katakana" skk-katakana-region skk-use-kakasi]
     ["Romaji" skk-romaji-region skk-use-kakasi]
     ["Zenkaku" skk-jisx0208-latin-region skk-use-kakasi])
    ["Count Jisyo Candidates" skk-count-jisyo-candidates t]
    ["Save Jisyo" skk-save-jisyo t]
    ["Undo Kakutei" skk-undo-kakutei t]
    ["Version" skk-version t])
  "Menu used in SKK mode.")

;;; SKK-CURSOR.EL related.
(defcustom skk-cursor-default-color
  (cond
   ((eq skk-emacs-type 'xemacs)
    (frame-property (selected-frame) 'cursor-color))
   (t
    (cdr (assq 'cursor-color (frame-parameters (selected-frame))))))
  "*SKK $B%b!<%I$N%*%U$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :group 'skk-decoration
  :group 'skk-cursor)

(defcustom skk-cursor-hiragana-color (if (eq skk-background-mode 'light)
					 "coral4"
				       "pink")
  "*$B$+$J%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-decoration
  :group 'skk-cursor)

(defcustom skk-cursor-katakana-color (if (eq skk-background-mode 'light)
					 "forestgreen"
				       "green")
  "*$B%+%?%+%J%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-decoration
  :group 'skk-cursor)

(defcustom skk-cursor-jisx0201-color (if (eq skk-background-mode 'light)
					 "blueviolet"
				       "thistle")
  "*JISX0201 $B%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-decoration
  :group 'skk-cursor)

(defcustom skk-cursor-jisx0208-latin-color "gold"
  "*$BA43Q1Q;z%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-decoration
  :group 'skk-cursor)

(defcustom skk-cursor-latin-color (if (eq skk-background-mode 'light)
				      "ivory4"
				    "gray")
  "*$B%"%9%-!<%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-decoration
  :group 'skk-cursor)

(defcustom skk-cursor-abbrev-color "royalblue"
  "*abbrev $B%b!<%I$r<($9%+!<%=%k?'!#(B
`skk-use-color-cursor' $B$,(B non-nil $B$N$H$-$K;HMQ$5$l$k!#(B"
  :type 'string
  :group 'skk-decoration
  :group 'skk-cursor)

;;; SKK-GADGET.EL related.
(defcustom skk-gengo-alist
  '((heisei "$BJ?@.(B" "H") (showa "$B><OB(B" "S") (taisho "$BBg@5(B" "T")
    (meiji "$BL@<#(B" "M"))
  "*$B859f$rI=5-$7$?J8;zNs$N(B alist$B!#(B
car $B$O859f$r%m!<%^;zI=5-$7$?(B symbol$B!#(B
cdr $B$O859fI=5-$N(B string $B$+$i$J$k%j%9%H!#(B"
  :type '(repeat (choice symbol string))
  :group 'skk-gadget)

(defcustom skk-month-alist
  '(("Jan" "1" "Januar") ("Feb" "2" "Februar") ("Mar" "3" "M,Ad(Brz")
    ("Apr" "4" "April") ("May" "5" "Mai")
    ("Jun" "6" "Juni") ("Jul" "7" "Juli") ("Aug" "8" "August")
    ("Sep" "9" "September") ("Oct" "10" "Oktober")
    ("Nov" "11" "November") ("Dec" "12" "Dezember"))
  "*$B7nL>$N1Q8lI=5-$H$=$NB>$NI=5-K!$NO"A[%j%9%H!#(B
$B3F(B cons cell $B$N(B car $B$O(B Emacs $B$NI8=`4X?t(B `current-time-string' $B$,JV$97A<0!#(B
cdr $B$OBP1~$9$kG$0U$N7A<0!#(B"
  :type '(repeat (repeat string))
  :group 'skk-gadget)

(defcustom skk-day-of-week-alist
  '(("Sun" "$BF|(B" "So") ("Mon" "$B7n(B" "Mo") ("Tue" "$B2P(B" "Di") ("Wed" "$B?e(B" "Mi")
    ("Thu" "$BLZ(B" "Do") ("Fri" "$B6b(B" "Fr") ("Sat" "$BEZ(B" "Sa"))
  "*$BMKF|$N1Q8lI=5-$H$=$NB>$NL>$NI=5-K!$NO"A[%j%9%H!#(B
$B3F(B cons cell $B$N(B car $B$O(B Emacs $B$NI8=`4X?t(B `current-time-string' $B$,JV$97A<0!#(B
cdr $B$OBP1~$9$kG$0U$N7A<0!#(B"
  :type '(repeat (repeat string))
  :group 'skk-gadget)

(defcustom skk-default-current-date-function
  (function
   (lambda (date-information format gengo and-time)
     (skk-default-current-date date-information nil 0 gengo 0 0 0 and-time)))
  "*`skk-current-date' $B$G%3!<%k$5$l$k%G%#%U%)%k%H$N4X?t!#(B
$B;~4V>pJs$r0z?t$K<h$j2C9)$7$?J8;zNs$r=PNO$9$k!#(B

$B0z?t$O(B DATE-INFORMATION, FORMAT, GENGO, AND-TIME $B$N(B 4 $B$D!#(B
DATE-INFORMATION $B$O(B `current-time-string' $B$,JV$7$?J8;zNs$r(B

  (year month day day-of-week hour minute second)

$B$N7A<0$GJQ49$7$?%j%9%H(B ($B3FMWAG$OJ8;zNs(B)$B!#(B
FORMAT $B$O(B `format' $B$NBh0l0z?t$NMM<0$K$h$k=PNO7ABV$r;XDj$9$kJ8;zNs!#(B
GENGO $B$O859fI=<($9$k$+$I$&$+(B (boolean)$B!#(B
AND-TIME $B$O;~9o$bI=<($9$k$+$I$&$+(B (boolean)$B!#(B"
  :type '(choice function (const nil))
  :group 'skk-gadget)

(defcustom skk-date-ad nil
  "*Non-nil $B$G$"$l$P!"(B`skk-today', `skk-clock' $B$G@>NqI=<($9$k!#(B
nil $B$G$"$l$P!"859fI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-gadget)

(defcustom skk-number-style 1
  "*`skk-today', `skk-clock' $B$GI=<($9$k?t;z$N7A<0$rJQ2=$5$;$k!#(B
$BH>3QI=(B: nil $B$b$7$/$O(B 0.
$BA43QI=<((B: t $B$b$7$/$O!"(B1.
$B4A?t;zI=(B: t, 0, 1 $B0J30$N(B non-nil $BCM!#(B"
  :type '(choice (choice :tag "Hankaku" (const nil) (integer 0))
		 (choice :tag "Zenkaku" (const t) (integer 1))
		 (integer :tag "Kansuuji" 3))
  :group 'skk-gadget)

(defcustom skk-units-alist
  '(("mile" ("km" . 1.6093) ("yard" . 1760))
    ("yard" ("feet" . 3) ("cm" . 91.44))
    ("feet" ("inch" . 12) ("cm" . 30.48))
    ("inch" ("feet" . 0.5) ("cm" . 2.54)))
  "*$BC10L49;;>pJs$NO"A[%j%9%H!#(B
$B3FMWAG$O(B ($B4p=`$H$J$kC10L(B ($BJQ49$9$kC10L(B . $BJQ49;~$NG\N((B)) $B$N7A<0$K$h$k!#(B
`skk-gadget-units-conversion' $B$G;2>H$9$k!#(B"
  :type 'sexp
  :group 'skk-gadget)

(defcustom skk-gadget-load-hook nil
  "*skk-gadget.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-hooks-and-functions
  :group 'skk-gadget)

;;; SKK-ISEARCH.EL related.
(defcustom skk-isearch-mode-string-alist
  '((hiragana . "[$B$+(B] ") (katakana . "[$B%+(B] ") (jisx0208-latin . "[$B1Q(B] ")
    (latin . "[aa] ") (abbrev . "[a$B$"(B] ") (nil . "[--] "))
  ;;  "*Alist of \(MODE-SYMBOL . PROMPT-STRING\).
  ;;MODE-SYMBOL is a symbol indicates canonical mode of skk for skk-isearch.
  ;;Valid MODE-SYMBOL is one of `hiragana', `katakana', `jisx0208-latin',
  ;;`latin' or nil.
  ;;PROMPT-STRING is a string used in prompt to indicates current mode of
  ;;skk for skk-isearch. "
  "*isearch $B;~$KF~NO%b!<%I$K=>$$=P$9%W%m%s%W%H;XDj$N$?$a$NO"A[%j%9%H!#(B
$B3FMWAG$O!"(B

  (MODE-SYMBOL . PROMPT-STRING)

$B$H$$$&(B cons cell$B!#(B
MODE-SYMBOL $B$OF~NO%b!<%I$rI=$o$9%7%s%\%k$G!"(B
$B2<5-$N$$$:$l$+$r;XDj$9$k!#(B

   $B$+$J%b!<%I!'(B `hiragana'
   $B%+%J%b!<%I!'(B `katakana'
   $BA41Q%b!<%I!'(B `jisx0208-latin'
   $B%"%9%-!<%b!<%I!'(B `latin'

nil $B$O!"(BSKK $B%b!<%I%*%U$rI=$o$9!#(B
PROMPT-STRING $B$O!"3:Ev$N(B SKK $B%b!<%I$KBP$7=P$9%W%m%s%W%H$NJ8;zNs!#(B"
  :type '(repeat
	  (cons (symbol :tag "Mode Name")
		(string :tag "Prompt for this mode")))
  :group 'skk-decoration
  :group 'skk-isearch)

(defcustom skk-isearch-start-mode nil
  ;;  "*Specifies the search mode when isearch is called.
  ;;This variable is valid only when `skk-isearch-use-previous-mode' is nil.
  ;;If nil, it means that if skk-mode has been called in this buffer, same as
  ;;the mode of the buffer, otherwise perform ascii search.
  ;;If `latin' or `ascii' perfrom ascii search.
  ;;If `hiragana', `hirakana' or `kana' -> hira kana search.
  ;;If `jisx0208-latin' or `eiji', perform zenkaku eiji (i.e. JIS X0208
  ;;alphabet) search."
  "*$B%+%l%s%H%P%C%U%!$G(B isearch $B$r9T$&:]$NF~NO%b!<%I!#(B
`skk-isearch-use-previous-mode' $B$,(B nil $B$N>l9g$N$_M-8z!#(B
isearch $B$r9T$&>l9g!">o$K$3$NJQ?t$G;XDj$7$?F~NO%b!<%I$,;HMQ$5$l$k(B ($B%f!<%6!<$,(B
$BL@<(E*$KJQ99$r9T$&$3$H$O2D(B)$B!#(B
$B2<5-$N$$$:$l$+$N%7%s%\%k$G;XDj$9$k!#(B

   nil:  $B%+%l%s%H%P%C%U%!$G(B SKK $B%b!<%I$,5/F0$5$l$F$$$l$P$=$N%b!<%I!"(B
         $B5/F0$5$l$F$$$J$1$l$P(B $B%"%9%-!<%b!<%I!#(B
   `hiragana' (`hiragana' or `kana'): $B$+$J%b!<%I(B
   `jisx0208-latin' (`eiji') : $BA41Q%b!<%I(B
   `latin' (`ascii'): $B%"%9%-!<%b!<%I(B"
  :type '(choice (const :tag "Succeed an input mode of current buffer" nil)
		 (const :tag "Ascii search" latin)
		 (const :tag "Hiragana search" hiragana)
		 (const :tag "JISX0208 alphabet search" jisx0208-latin))
  :group 'skk-isearch)

(defcustom skk-isearch-use-previous-mode nil
  ;; "*Non-nil means use the same search mode as that of the last search."
  "*Non-nil $B$G$"$l$P!"F1$8%P%C%U%!$G$N:G8e$N8!:w;~$N%b!<%I$r;HMQ$9$k!#(B"
  :type 'boolean
  :group 'skk-isearch)

(defcustom skk-isearch-initial-mode-when-skk-mode-disabled 'latin
  ;;  "*Symbol indicates the mode to use as initial mode for skk-isearch when
  ;;skk is turned off in the current buffer."
  "*SKK $B%b!<%I$,%*%U$N%P%C%U%!$G!":G=i$K(B isearch $B$r9T$&:]$NF~NO%b!<%I!#(B"
  :type '(choice (const :tag "Ascii search" latin)
		 (const :tag "Hiragana search" hiragana)
		 (const :tag "JISX0208 alphabet search" jisx0208-latin))
  :group 'skk-isearch)

(defcustom skk-isearch-whitespace-regexp "\\(\\s \\|[ \t\n\r\f]\\)*"
  ;;  "*Regular expression to match a sequence of whitespace chars.
  ;;This applies to regular expression incremental search."
  "$B6uGrJ8;z$NO"B3$H$7$F%^%C%A$5$;$k$Y$-@55,I=8=!#(B
regexp isearch $B$N:]!"$3$N@55,I=8=$K%^%C%A$9$kJ8;z$,8!:wJ8;zNs$N4V$K4^$^$l$F$$$F(B
$B$b%^%C%A$9$k!#(B"
  :type 'regexp
  :group 'skk-isearch)

;;; SKK-JISX0201.EL related.
(defcustom skk-jisx0201-mode-string " jisx0201"
  "*SKK $B$,(B JISX0201 $B%b!<%I$G$"$k$H$-$K%b!<%I%i%$%s$KI=<($5$l$kJ8;zNs!#(B"
  :type 'string
  :group 'skk-hooks-and-functions
  :group 'skk-jisx0201)

;;; SKK-KAKASI.EL related.
(defcustom skk-use-kakasi (exec-installed-p "kakasi")
  "*Non-nil $B$G$"$l$P(B KAKASI $B$r;H$C$?JQ49$r9T$&!#(B"
  :type 'boolean
  :group 'skk-kakasi)

(defcustom skk-kakasi-command (exec-installed-p "kakasi")
  "*KAKASI $B%3%^%s%IK\BN!#(B"
  :type 'file
  :group 'skk-filenames
  :group 'skk-kakasi)

(defcustom skk-romaji-*-by-hepburn t
  "*Non-nil $B$G$"$l$P(B KAKASI $B$r;H$C$?%m!<%^;z$X$NJQ49MM<0$K%X%\%s<0$rMQ$$$k!#(B
$BNc$($P!"(B
  \"$B$7(B\" -> \"shi\"

nil $B$G$"$l$P!"71Na<0(B \"($B!VF|K\<0!W$H$b8@$&$h$&$@(B)\" $B$rMQ$$$k!#(B
$BNc$($P!"(B
   \"$B$7(B\" -> \"si\"

$B><OB(B 29 $BG/(B 12 $B7n(B 9 $BF|IUFb3U9p<(Bh0l9f$K$h$l$P!"86B'E*$K71Na<0(B \"($BF|K\<0(B)\" $B$r(B
$BMQ$$$k$+$N$h$&$K5-:\$5$l$F$$$k$,!":#F|0lHLE*$J5-:\J}K!$O!"$`$7$m!"%X%\%s<0$G$"(B
$B$k$h$&$K;W$&!#(B"
  :type 'boolean
  :group 'skk-kakasi)

(defcustom skk-kakasi-load-hook nil
  "*skk-kakasi.el $B$,%m!<%I$5$l$?$H$-$N%U%C%/!#(B"
  :type 'hook
  :group 'skk-hooks-and-functions
  :group 'skk-kakasi)

(defcustom skk-gyakubiki-jisyo-list nil
  "KAKASI $B$r;H$C$?JQ49$N:]$K;2>H$9$k5U0z$-<-=q$N%j%9%H!#(B
nil $B$J$i(B KAKASI $B%3%^%s%I$OI8=`$N<-=q$r;2>H$9$k!#(B"
  :type '(repeat file)
  :group 'skk-kakasi)

;;; SKK-KCODE.EL related.
(defcustom skk-input-by-code-menu-keys1 '(?a ?s ?d ?f ?g ?h ?q ?w ?e ?r ?t ?y)
  "*$B%a%K%e!<7A<0$G(B JIS $BJ8;z$rF~NO$9$k$H$-$K;HMQ$9$kA*Br%-!<$N%j%9%H!#(B
$BBh(B 1 $BCJ3,$N%a%K%e!<$G;HMQ$9$k!#(B
12 $B8D$N%-!<(B (char type) $B$r4^$`I,MW$,$"$k!#(B"
  :type '(repeat character)
  :group 'skk-keybinds
  :group 'skk-kcode)

(defcustom skk-input-by-code-menu-keys2
  '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u)
  "*$B%a%K%e!<7A<0$G(B JIS $BJ8;z$rF~NO$9$k$H$-$K;HMQ$9$kA*Br%-!<$N%j%9%H!#(B
$BBh(B 2 $BCJ3,$N%a%K%e!<$G;HMQ$9$k!#(B
16 $B8D$N%-!<(B (char type) $B$r4^$`I,MW$,$"$k!#(B"
  :type '(repeat character)
  :group 'skk-keybinds
  :group 'skk-kcode)

(defcustom skk-kcode-charset
  (cond ((featurep 'jisx0213) 'japanese-jisx0213-1)
	((memq skk-emacs-type '(xemacs mule5 mule4 mule3))
	 'japanese-jisx0208)
	(t lc-jp))
  "*skk-input-by-code-or-menu $B$G;H$o$l$kJ8;z%;%C%H!#(B"
  :type 'symbol
  :group 'skk-kcode)

(defcustom skk-kcode-load-hook nil
  "*skk-kcode.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-hooks-and-functions
  :group 'skk-kcode)

;;; SKK-LOOK.EL related.
(defcustom skk-look-command (exec-installed-p "look")
  "*UNIX look $B%3%^%s%I$NL>A0!#(B"
  :type 'file
  :group 'skk-filenames
  :group 'skk-look)

(defcustom skk-look-ignore-case t
  "*Non-nil $B$G$"$l$P!"BgJ8;z!&>.J8;z$r6hJL$7$J$$$G8!:w$r9T$&!#(B
look $B%3%^%s%I$K%*%W%7%g%s(B \"-f\" $B$rEO$9!#(B"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-look-dictionary-order t
  "*Non-nil $B$G$"$l$P!"<-=q=g$K%=!<%H$5$l$?8!:w%U%!%$%k$r;HMQ$9$k!#(B
look $B%3%^%s%I$K%*%W%7%g%s(B \"-d\" $B$rEO$9!#(B"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-look-use-alternate-dictionary nil
  "*Non-nil $B$G$"$l$P!"(B/usr/dict/web2 $B$r;H$$8!:w$r9T$&!#(B
$B%G%#%U%)%k%H$N<-=q$O!"(B/usr/dict/words$B!#(B
look $B%3%^%s%I$K%*%W%7%g%s(B \"-a\" $B$rEO$9!#(B"
  :type '(choice file (const nil))
  :group 'skk-look)

(defcustom skk-look-termination-character nil
  "*UNIX look $B%3%^%s%I%*%W%7%g%s$N=*C<J8;zNs!#(B
look $B%3%^%s%I$K%*%W%7%g%s(B \"-t\" $B$H$=$NJ8;zNs$rEO$9!#(B
nil $B$G$"$l$P$3$N%*%W%7%g%s$O;HMQ$5$l$J$$!#(B"
  :type '(choice string (const nil))
  :group 'skk-look)

(defcustom skk-look-dictionary nil
  "*look $B%3%^%s%I$,8!:w$9$k<-=q%U%!%$%k!#(B
nil $B$G$"$l$P!"(B/usr/dict/words $B$r;HMQ$9$k!#(B"
  :type '(choice file (const nil))
  :group 'skk-filenames
  :group 'skk-look)

(defcustom skk-look-recursive-search nil "\
*Non-nil $B$G$"$l$P!"(Blook $B%3%^%s%I$,8+$D$1$?1QC18l$rJQ49%-!<$K$7!":F8!:w$r9T$&!#(B
$B:F8!:w$N7k2L!"8uJd$,8+$D$+$i$J$1$l$P!"85$N1QC18l<+?H$r8uJd$H$7$F=PNO$9$k!#(B"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-look-expanded-word-only t
  "\
*Non-nil $B$J$i$P(B look $B$N=PNO$KBP$9$k:F8!:w$,@.8y$7$?8uJd$N$_$rI=<($9$k!#(B
`skk-look-recursive-search' $B$,(B non-nil $B$G$"$k$H$-$N$_M-8z!#(B"
  :type 'boolean
  :group 'skk-look)

(defcustom skk-look-use-ispell (and (exec-installed-p "ispell")
				    (module-installed-p 'ispell))
  "*Non-nil $B$G$"$l$P!"(Blook $B$K$h$k8!:w$N:]!"(Bispell $B$rJ;MQ$9$k!#(B"
  :type 'boolean
  :group 'skk-look)

;;; SKK-NUM.EL related.
(defcustom skk-num-type-alist
  '((0 . identity)
    (1 . skk-num-jisx0208-latin)
    (2 . skk-num-type2-kanji)
    (3 . skk-num-type3-kanji)
    (4 . skk-num-recompute)
    (5 . skk-num-type5-kanji)
    (9 . skk-num-shogi))
  "*$B?tCM$NJQ49$N$?$a$N!"%$%s%G%/%9$HJQ49$K;HMQ$9$k4X?t$H$NO"A[%j%9%H!#(B
$B3FMWAG$O!"(B`($B%$%s%G%/%9(B . $B4X?tL>(B)' $B$H$$$&9=@.$K$J$C$F$$$k!#(B
$B%$%s%G%/%9$K$O!"Nc$($P8+=P$78l$,(B \"$BJ?@.(B#1$BG/(B\" $B$N$H$-!"(B`#' $B5-9f$ND>8e$KI=<((B
$B$5$l$k(B integer `1' $B$rBeF~$9$k!#(B

$B%$%s%G%/%9$H4X?t$N4X78(B ($B%G%#%U%)%k%HCM(B) $B$O2<5-$NDL$j!#(B
    0 -> $BL5JQ49(B
    1 -> $BA43Q?t;z$XJQ49(B
    2 -> $B4A?t;z$XJQ49(B ($B0L<h$j$J$7(B)
    3 -> $B4A?t;z$XJQ49(B ($B0L<h$j$r$9$k(B)
    4 -> $B$=$N?t;z$=$N$b$N$r%-!<$K$7$F<-=q$r:F8!:w(B
    5 -> $B4A?t;z(B ($B<j7A$J$I$G;HMQ$9$kJ8;z$r;HMQ(B) $B$XJQ49(B ($B0L<h$j$r$9$k(B)
    9 -> $B>-4}$G;HMQ$9$k?t;z(B (\"$B#3;M(B\" $B$J$I(B) $B$KJQ49(B"
  :type '(repeat (cons (choice :tag "Index"
			       (integer 0) (integer 1) (integer 2) (integer 3)
			       (integer 4) (integer 5) (integer 9))
		  (function :tag "Function")))
  :group 'skk-num)

(defcustom skk-num-convert-float nil
  "*Non-nil $B$G$"$l$P!"IbF0>.?tE@?t$r;H$C$?8+=P$78l$KBP1~$7$FJQ49$r9T$&!#(B
$B$3$NCM$r(B non-nil $B$K$9$k$3$H$G!"(B\"#.# /#1$B!%(B#1/#0$B7n(B#0$BF|(B/\" $B$J$I$N<-=q8+=P$7$,;HMQ(B
$B$G$-$J$/$J$k$N$G!"Cm0U!#(B"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-num-uniq (or (assq 4 skk-num-type-alist)
			    (and (assq 2 skk-num-type-alist)
				 (or (assq 3 skk-num-type-alist)
				     (assq 5 skk-num-type-alist)))) "\
*Non-nil $B$G$"$l$P!"0[$J$k?tCMI=8=$G$bJQ497k2L$,F1$8?tCM$r=EJ#$7$F=PNO$7$J$$!#(B"
  :type 'boolean
  :group 'skk-num)

(defcustom skk-num-load-hook nil
  "*skk-num.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-hooks-and-functions
  :group 'skk-num)

;;; SKK-SERVER.EL related.
(defcustom skk-server-host (or (getenv "SKKSERVER") "localhost")
  "*SKK $B<-=q%5!<%P!<$rAv$i$;$F$$$k%[%9%HL>!#(B"
  :type '(choice (string :tag "Name of the Host")
		 (const nil))
  :group 'skk-server)

(defcustom skk-server-prog (getenv "SKKSERV")
  "*SKK $B<-=q%5!<%P!<%W%m%0%i%`L>!#%U%k%Q%9$G=q$/!#(B"
  :type '(choice (file :tag "File Name of the Program")
		 (const nil))
  :group 'skk-filenames
  :group 'skk-server)

(defcustom skk-server-jisyo (getenv "SKK_JISYO")
  "*SKK $B<-=q%5!<%P!<%W%m%0%i%`$KEO$9<-=qL>!#%U%k%Q%9$G=q$/!#(B"
  :type '(choice (file :tag "File Name of the Dictionary")
		 (const nil))
  :group 'skk-filenames
  :group 'skk-server)

(defcustom skk-server-portnum (if (eq system-type 'windows-nt) 1178)
  "*Non-nil $B$G$"$l$P!"$=$NCM$r(B port number $B$H$7$F(B skkserv $B$H(B TCP $B@\B3$9$k!#(B
/etc/services $B$rD>@\=q$-49$($k8"8B$,$J$$%f!<%6!<$N$?$a$NJQ?t!#(B
Windows $B$G$O%G%#%U%)%k%HCM$H$7$F(B 1178 $B$,@_Dj$5$l$k!#(B"
  :type '(choice integer (const nil)))

;;(defvar skk-server-debug nil
;;  "*Non-nil $B$G$"$l$P!"<-=q%5!<%P!<%W%m%0%i%`$r%G%#%P%C%0%b!<%I$G5/F0$9$k!#(B
;;$B%G%#%P%C%0!&%b!<%I$G(B skkserv $B$rAv$i$;$k$H!"$=$N$^$^(B foreground $B$GAv$j!"(B
;;$B%a%C%;!<%8$r=PNO$9$k!#%-!<%\!<%I$+$i3d$j$3$_$r$+$1$k$3$H$b$G$-$k!#(B")

(defcustom skk-servers-list nil
  "*$B<-=q%5!<%P!<Kh$N>pJs%j%9%H!#(B

$BJ#?t$N%[%9%H$GF0$$$F$$$k%5!<%P$K%"%/%;%9$G$-$k>l9g$K$O!"0J2<$N$h$&$K%j%9%H$N(B
$B3FMWAG$K=g$K%[%9%HL>!"%U%k%Q%9$G$N(B SKK $B%5!<%P!<L>!"(BSKK $B%5!<%P!<$KEO$9<-=qL>!"(B
SKK $B%5!<%P!<$,;HMQ$9$k%]!<%HHV9f$r=q$-!"@_Dj$r$9$k$3$H$,$G$-$k!#(B

   (setq skk-servers-list
         '((\"host1\" \"/path/to/skkserv\" \"/path/to/SKK-JISYO.L\" 1178)
           (\"host2\" \"/path/to/skkserv\")))

$B$3$N>l9g!":G=i$K;XDj$7$?%5!<%P$K%"%/%;%9$G$-$J$/$J$k$H!"<+F0E*$K=g<!%j%9%H$K$"(B
$B$k;D$j$N%5!<%P$K%"%/%;%9$9$k$h$&$K$J$k!#(B
$B%5!<%P!<$N%G%#%U%)%k%H$N<-=q$*$h$S%]!<%HHV9f$r;HMQ$9$k>l9g$O(B nil $B$r;XDj$9$k$+!"(B
$B2?$b=q$+$J$$$GNI$$!#(B

$B$J$*!"%f!<%6!<<+?H$K<B9T8"8B$N$J$$%5!<%P!<$r;XDj$9$k>l9g$O!"(B

   (setq skk-servers-list '((\"host1\") (\"host2\")))

$B$N$h$&$K!"%[%9%HL>$@$1$r=q$/$3$H$,$G$-$k!#>e5-$N@_DjNc$G$O!"(Bhost1, host2 $B$K$*(B
$B$1$k(B skkserv $B%5!<%S%9$N(B TCP $B@\B3$N3+;O$N$_;n$_!"%5!<%P!<$N5/F0$O;n$_$J$$!#(B"
  :type '(repeat
	  (list (string :tag "Hostname")
		(choice :tag "Server" file (const nil))
		(choice :tag "Dictionary" file (const nil))
		(choice :tag "Port number" integer (const nil))))
  :group 'skk-server)

(defcustom skk-server-report-response nil
  "*Non-nil $B$G$"$l$P!"%5!<%P$N1~Ez>u67$rJs9p$9$k!#(B
$B6qBNE*$K$O!"JQ49;~%5!<%P!<$NAw=P$9$kJ8;z$r<u$1<h$k$^$G$K(B
`accept-process-output' $B$r2?2s<B9T$7$?$+$rJs9p$9$k!#(B"
  :type 'boolean
  :group 'skk-server)

(defcustom skk-server-remote-shell-program
  (or (getenv "REMOTESHELL")
      (and (boundp 'remote-shell-program)
	   (symbol-value 'remote-shell-program))
      (cond
       ((eq system-type 'berkeley-unix)
	(if (file-exists-p "/usr/ucb/rsh")
	    "/usr/ucb/rsh"
	  "/usr/bin/rsh"))
       ((eq system-type 'usg-unix-v)
	(if (file-exists-p "/usr/ucb/remsh")
	    "/usr/ucb/remsh"
	  "/bin/rsh"))
       ((eq system-type 'hpux)
	"/usr/bin/remsh")
       ((eq system-type 'EWS-UX/V)
	"/usr/ucb/remsh")
       ((eq system-type 'pcux)
	"/usr/bin/rcmd")
       (t
	"rsh")))
  "*$B%j%b!<%H%7%'%k$N%W%m%0%i%`L>!#(B"
  :type 'file
  :group 'skk-filenames
  :group 'skk-server)

(defcustom skk-server-load-hook nil
  "*skk-server.el $B$r%m!<%I$7$?8e$K%3!<%k$5$l$k%U%C%/!#(B"
  :type 'hook
  :group 'skk-hooks-and-functions
  :group 'skk-server)

(defcustom skk-jisx0213-prohibit nil
  "*Non-nil $B$G$"$l$P(B JISX0213 $B$NJ8;zNs$r4^$`8uJd$N=PNO$r$7$J$$!#(B
Mule-UCS $B$,%$%s%9%H!<%k$5$l$F$$$J$$$H$-$O$3$NCM$OF0:n$K1F6A$7$J$$!#(B"
  :type 'boolean
  :group 'skk-misc)

;;; SKK-TUT.EL related.
(defcustom skk-tut-file
  (cond ((eq skk-emacs-type 'xemacs)
	 (locate-data-file "SKK.tut"))
	(t
	 "/usr/local/share/skk/SKK.tut"))
  "*SKK $B%A%e!<%H%j%"%k$N%U%!%$%kL>!#(B
The English version is SKK.tut.E."
  :type 'file
  :group 'skk-filenames
  :group 'skk-tut)

(defvar skk-tut-file-alist
  (` (("Japanese" . (, skk-tut-file))
      ("English" . (, (concat skk-tut-file ".E")))))
  "*Alist of `(LANGUAGE . TUTORIAL-FILE)' pairs.")

(defcustom skk-tut-use-face skk-use-face
  "*Non-nil $B$G$"$l$P!"%A%e!<%H%j%"%k$G(B face $B$rMxMQ$7$?I=<($r9T$&!#(B"
  :type 'boolean
  :group 'skk-decoration
  :group 'skk-tut)

(defface skk-tut-section-face
  '((((class color) (background light))
     (:foreground "yellow" :background "dodgerblue"))
    (((class color) (background dark))
     (:foreground "yellow" :background "slateblue"))
    (((class grayscale)) (:bold t) (:italic t)))
  "*$B%A%e!<%H%j%"%kCf$N%;%/%7%g%s$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-decoration
  :group 'skk-tut)

(defface skk-tut-do-it-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (((class grayscale)) (:bold t)))
  "*$B%A%e!<%H%j%"%kCf$N;X<(9`L\$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-decoration
  :group 'skk-tut)

(defface skk-tut-question-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (((class grayscale)) (:underline t)))
  "*$B%A%e!<%H%j%"%kCf$NLdBj$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-decoration
  :group 'skk-tut)

(defface skk-tut-key-bind-face
  '((((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "OrangeRed"))
    (((class grayscale)) (:bold t)))
  "*$B%A%e!<%H%j%"%kCf$N%-!<%P%$%s%I$NI=<(ItJ,$N(B face$B!#(B"
  :group 'skk-decoration
  :group 'skk-tut)

(defface skk-tut-hint-face
  '((((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (((class grayscale)) (:italic t)))
  "*$B%A%e!<%H%j%"%kCf$N%R%s%H$NI=<(ItJ,$N(B face$B!#(B
$B8=:_$N$H$3$m!"(BSKK.tut.E $B$G$7$+;HMQ$5$l$F$$$J$$!#(B"
  :group 'skk-decoration
  :group 'skk-tut)

;;; -- INTERNAL CONSTANTS AND VARIABLES of SKK.EL
;; (ones of other separate programs should be in the relative files.)
;; ---- global ones.
;;(defvar skk-henkan-face 'skk-henkan-face)
(defconst skk-coding-system-alist
  (cond ((featurep 'jisx0213)
	 '(("euc" . euc-jisx0213)
	   ("ujis" . euc-jisx0213)
	   ("sjis". shift_jisx0213)
	   ("jis" . iso-2022-jp-3-strict)))
	((memq skk-emacs-type '(xemacs mule5 mule4 mule3))
	 '(("euc" . euc-japan)
	   ("ujis" . euc-japan)
	   ("sjis". shift_jis)
	   ("jis" . junet)))
	(t
	 '(("euc" . *euc-japan*)
	   ("ujis" . *euc-japan*)
	   ("sjis". *sjis*)
	   ("jis" . *junet*))))
  "coding-system $B$NJ8;zNsI=8=$H!"%7%s%\%kI=8=$NO"A[%j%9%H!#(B")

(defconst skk-kana-rom-vector
  ["x" "a" "x" "i" "x" "u" "x" "e" "x" "o" "k" "g" "k" "g" "k" "g"
   "k" "g" "k" "g" "s" "z" "s" "j" "s" "z" "s" "z" "s" "z" "t" "d"
   "t" "d" "t" "t" "d" "t" "d" "t" "d" "n" "n" "n" "n" "n" "h" "b"
   "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "h" "b" "p" "m" "m" "m"
   "m" "m" "x" "y" "x" "y" "x" "y" "r" "r" "r" "r" "r" "x" "w" "x"
   "x" "w" "n"]
  "*$B$+$JJ8;z$+$i%m!<%^;z$X$NJQ49%k!<%k!#(B
$B2<5-$N3:Ev$9$k$+$JJ8;z$r$=$NJ8;z$N%m!<%^;z%W%l%U%#%C%/%9$G8=$o$7$?$b$N!#(B
    $B$!(B  $B$"(B  $B$#(B  $B$$(B  $B$%(B  $B$&(B  $B$'(B  $B$((B  $B$)(B  $B$*(B  $B$+(B  $B$,(B  $B$-(B  $B$.(B  $B$/(B  $B$0(B
    $B$1(B  $B$2(B  $B$3(B  $B$4(B  $B$5(B  $B$6(B  $B$7(B  $B$8(B  $B$9(B  $B$:(B  $B$;(B  $B$<(B  $B$=(B  $B$>(B  $B$?(B  $B$@(B
    $B$A(B  $B$B(B  $B$C(B  $B$D(B  $B$E(B  $B$F(B  $B$G(B  $B$H(B  $B$I(B  $B$J(B  $B$K(B  $B$L(B  $B$M(B  $B$N(B  $B$O(B  $B$P(B
    $B$Q(B  $B$R(B  $B$S(B  $B$T(B  $B$U(B  $B$V(B  $B$W(B  $B$X(B  $B$Y(B  $B$Z(B  $B$[(B  $B$\(B  $B$](B  $B$^(B  $B$_(B  $B$`(B
    $B$a(B  $B$b(B  $B$c(B  $B$d(B  $B$e(B  $B$f(B  $B$g(B  $B$h(B  $B$i(B  $B$j(B  $B$k(B  $B$l(B  $B$m(B  $B$n(B  $B$o(B  $B$p(B
    $B$q(B  $B$r(B  $B$s(B"
  ;; (length skk-kana-rom-vector)
  ;; --> 83
  ;; (setq kana '("$B$!(B" "$B$"(B" "$B$#(B" "$B$$(B" "$B$%(B" "$B$&(B" "$B$'(B" "$B$((B" "$B$)(B" "$B$*(B"
  ;;		  "$B$+(B" "$B$,(B" "$B$-(B" "$B$.(B" "$B$/(B" "$B$0(B" "$B$1(B" "$B$2(B" "$B$3(B" "$B$4(B"
  ;;	          "$B$5(B" "$B$6(B" "$B$7(B" "$B$8(B" "$B$9(B" "$B$:(B" "$B$;(B" "$B$<(B" "$B$=(B" "$B$>(B"
  ;;	          "$B$?(B" "$B$@(B" "$B$A(B" "$B$B(B" "$B$C(B" "$B$D(B" "$B$E(B" "$B$F(B" "$B$G(B" "$B$H(B" "$B$I(B"
  ;;		  "$B$J(B" "$B$K(B" "$B$L(B" "$B$M(B" "$B$N(B" "$B$O(B" "$B$P(B" "$B$Q(B" "$B$R(B" "$B$S(B" "$B$T(B"
  ;;		  "$B$U(B" "$B$V(B" "$B$W(B" "$B$X(B" "$B$Y(B" "$B$Z(B" "$B$[(B" "$B$\(B" "$B$](B"
  ;;		  "$B$^(B" "$B$_(B" "$B$`(B" "$B$a(B" "$B$b(B" "$B$c(B" "$B$d(B" "$B$e(B" "$B$f(B" "$B$g(B" "$B$h(B"
  ;;	          "$B$i(B" "$B$j(B" "$B$k(B" "$B$l(B" "$B$m(B" "$B$n(B" "$B$o(B" "$B$p(B" "$B$q(B" "$B$r(B" "$B$s(B"))
  ;; (length kana)
  ;; --> 83
  ;; (mapcar (lambda (s) (- (char-octet (string-to-char s) 1) 33))
  ;;	kana)
  ;; --> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25\
  ;;      26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48\
  ;;      49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71\
  ;;      72 73 74 75 76 77 78 79 80 81 82)
  )

(defconst skk-default-jisx0208-latin-vector
  ;; note that skk-jisx0208-latin-vector is a user variable.
  ;; skk.el $B%m!<%IA0$K(B .emacs $B$J$I$G!"(Bskk-jisx0208-latin-vector $B$NJL$NCM$r%f!<(B
  ;; $B%6!<$,D>@\=q$$$?$j!"(Bskk.el $B%m!<%I8e$K$3$NCM$r(B aset $B$GD>@\$$$8$C$?$j$7$J(B
  ;; $B$1$l$P(B default-value $B$G(B skk-jisx0208-latin-vector $B$K%"%/%;%9$9$k$3$H$G(B
  ;; skk-default-jisx0208-latin-vector $B$NCM$rJ];}$9$k$3$H$b$G$-$h$&$,!"$=$l$O(B
  ;; $BK>$a$J$$(B...$B!#(B
  [nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "$B!!(B"  "$B!*(B" "$B!I(B" "$B!t(B" "$B!p(B" "$B!s(B" "$B!u(B" "$B!G(B"
   "$B!J(B" "$B!K(B" "$B!v(B" "$B!\(B" "$B!$(B" "$B!](B" "$B!%(B" "$B!?(B"
   "$B#0(B" "$B#1(B" "$B#2(B" "$B#3(B" "$B#4(B" "$B#5(B" "$B#6(B" "$B#7(B"
   "$B#8(B" "$B#9(B" "$B!'(B" "$B!((B" "$B!c(B" "$B!a(B" "$B!d(B" "$B!)(B"
   "$B!w(B" "$B#A(B" "$B#B(B" "$B#C(B" "$B#D(B" "$B#E(B" "$B#F(B" "$B#G(B"
   "$B#H(B" "$B#I(B" "$B#J(B" "$B#K(B" "$B#L(B" "$B#M(B" "$B#N(B" "$B#O(B"
   "$B#P(B" "$B#Q(B" "$B#R(B" "$B#S(B" "$B#T(B" "$B#U(B" "$B#V(B" "$B#W(B"
   "$B#X(B" "$B#Y(B" "$B#Z(B" "$B!N(B" "$B!@(B" "$B!O(B" "$B!0(B" "$B!2(B"
   "$B!F(B" "$B#a(B" "$B#b(B" "$B#c(B" "$B#d(B" "$B#e(B" "$B#f(B" "$B#g(B"
   "$B#h(B" "$B#i(B" "$B#j(B" "$B#k(B" "$B#l(B" "$B#m(B" "$B#n(B" "$B#o(B"
   "$B#p(B" "$B#q(B" "$B#r(B" "$B#s(B" "$B#t(B" "$B#u(B" "$B#v(B" "$B#w(B"
   "$B#x(B" "$B#y(B" "$B#z(B" "$B!P(B" "$B!C(B" "$B!Q(B" "$B!A(B" nil]
  "skk-jisx0208-latin-region $B$G;2>H$9$kJ8;z%F!<%V%k!#(B
\"ascii\" -> \"$B#a#s#c#i#i(B\" $B$N$h$&$JA43QJ8;z$X$NJQ49$r9T$&:]$KMxMQ$9$k!#(B")

(defconst skk-kanji-len (length "$B$"(B")
  "$B4A;z0lJ8;z$ND9$5!#(BMule[1-3] $B$G$O(B 3 $B$K$J$k!#(BMule4, XEmacs $B$G$O(B 1$B!#(B")

(defconst skk-hankaku-alist
  (when (eq skk-emacs-type 'mule2)
    '((161 . 32)	; ?\
      (170 . 33)	;?\!
      (201 . 34)	;?\"
      (244 . 35)	;?\#
      (240 . 36)	;?\$
      (243 . 37)	;?\%
      (245 . 38)	;?\&
      (199 . 39)	;?\'
      (202 . 40)	;?\(
      (203 . 41)	;?\)
      (246 . 42)	;?\*
      (220 . 43)	;?\+
      (164 . 44)	;?\,
      (221 . 45)	;?\-
      (165 . 46)	;?\.
      (191 . 47)	;?\/
      (167 . 58)	;?\:
      (168 . 59)	;?\;
      (227 . 60)	;?\<
      (225 . 61)	;?\=
      (228 . 62)	;?\>
      (169 . 63)	;?\?
      (247 . 64)	;?\@
      (206 . 91)	;?\[
      (239 . 92)	;?\\
      (207 . 93)	;?\]
      (176 . 94)	;?^
      (178 . 95)	;?\_
      (208 . 123)	;?\{
      (195 . 124)	;?\|
      (209 . 125)	;?\}
      (177 . 126)	;?\~
      (198 . 96)))	;?`
  "$BJ8;z%3!<%I$N(B 2 $BHVL\$N%P%$%H$HBP1~$9$k(B ascii $BJ8;z(B (char) $B$H$NO"A[%j%9%H!#(B
Mule 2 $B$r;HMQ$9$k>l9g$K(B `skk-latin-region' $B$G;2>H$9$k!#(B
Mule-2.3 $BE:IU$N(B egg.el $B$h$j%3%T!<$7$?!#(B")

(defconst skk-kana-cleanup-command-list
  '(skk-undo
    skk-kakutei
    skk-delete-backward-char
    skk-insert
    skk-previous-candidate))

(defconst skk-quote-char-alist
  '((?\; . "\\073")
    (?/ . "\\057")
    (?\n . "\\n")
    (?\r . "\\r")
    (?\" . "\\\"")
    (?\\  . "\\\\"))
  "$B<-=q%(%s%H%jFb$K4^$a$F$O$J$i$J$$J8;z$rCV$-JQ$($k$?$a$NO"A[%j%9%H!#(B
`;' $B$O!"Cp<a$H4X78$J$$>l9g$@$1CV49$9$k!#(B")

(defvar skk-emacs-id nil
  "$BJ#?t(B emacs $B$r<1JL$9$kJ8;zNs!#(B
$B%f!<%6!<<-=q$rJ#?t$N(B emacs $B>e$G5/F0$5$l$F$$$k(B SKK $B$G6&M-$9$k$H$-$K;2>H$9$k!#(B")

(defvar skk-jisyo-update-vector nil
  "$BD9$5$,(B skk-jisyo-save-count $B$N%Y%/%H%k!#(B
$B<-=q%P%C%U%!99?7$N5-O?$rJ]B8$7!"<-=q%P%C%U%!$r<-=q%U%!%$%k$K%;!<%V$9$k$H$-$K!"(B
$BB>$N(B SKK $B$,<-=q%U%!%$%k$K:G6a%"%/%;%9$7$F$$$k$H$-$K$O!"<-=q%U%!%$%k$r%P%C%U%!(B
$B$KFI$_9~$s$G$+$i!"(Bskk-jisyo-update-vector $B$rMQ$$$F%P%C%U%!$r99?7$7!"$=$N(B
$B7k2L$r%U%!%$%k$K%;!<%V$9$k!#(B")

(defvar skk-rule-tree nil
  "$B%m!<%^;z(B -> $B$+$JJQ49$N>uBVA+0\5,B'$rI=$9%D%j!<$N=i4|>uBV!#(B
$B:G=i$K(B skk-mode $B$r5/F0$7$?$H$-$K(B skk-rom-kana-base-rule-list $B$H(B
skk-rom-kana-rule-list $B$+$iLZ$N7A$K%3%s%Q%$%k$5$l$k!#(B
\\[skk-restart] $B$K$h$C$F$b:F%3%s%Q%$%k$5$l$k!#(B")

(defvar skk-insert-new-word-function nil
  "$B8uJd$rA^F~$7$?$H$-$K(B funcall $B$5$l$k4X?t$rJ]B8$9$kJQ?t!#(B")

(defvar skk-isearch-message nil
  "skk-isearch $B4X?t$r%3!<%k$9$k$?$a$N%U%i%0!#(B
Non-nil $B$G$"$l$P!"(B`skk-isearch-message' $B4X?t$r%3!<%k$9$k!#(B")

(defvar skk-mode-invoked nil
  "Non-nil $B$G$"$l$P!"(BEmacs $B$r5/F08e4{$K(B `skk-mode' $B$r5/F0$7$?$3$H$r<($9!#(B")

(defvar skk-kakutei-count 0
  "$BJQ498uJd$r3NDj$7$?%+%&%s%H$rJ];}$9$kJQ?t!#(B
`skk-record-file' $B$N(B \"$B3NDj(B:\" $B9`L\$N%+%&%s%?!<!#(B")

(defvar skk-touroku-count 0
  "$B<-=qEPO?$7$?%+%&%s%H$rJ];}$9$kJQ?t!#(B
`skk-record-file' $B$N(B \"$BEPO?(B:\" $B9`L\$N%+%&%s%?!<!#(B")

(defvar skk-update-jisyo-count 0
  "$B<-=q$r99?7$7$?2s?t!#(B
$B$3$N%+%&%s%?!<$N?t;z$,(B `skk-jisyo-save-count' $B0J>e$H$J$C$?$H$-$K%f!<%6!<<-=q$N(B
$B%*!<%H%;!<%V$,9T$o$l$k!#(B
$B<-=q$N%;!<%V$,9T$o$l$k$H%$%K%7%c%i%$%:$5$l$k!#(B")

(defvar skk-kakutei-history nil
  "$BAw$j$J$7$G3NDj$5$l$?8+=P$78l!&8uJd$NMzNr!#(B

   (\"$B$_$@$7$4(B\" . \"$B8+=P$78l(B\")

   $B$H$$$&7A<0$NO"A[%j%9%H!#(B")

(defvar skk-minibuffer-origin-mode nil
  "$BF~NO%b!<%I$rI=$o$9%7%s%\%k!#(B
$BM-8z$JCM$O!"(B`hiragana', `katakana', `abbrev', `latin', `jisx0208-latin'
$B$b$7$/$O(B nil $B$N$$$:$l$+!#(B")

(defvar skk-menu nil)

(skk-deflocalvar skk-modeline-input-mode nil)
(defvar skk-indicator-alist nil)

;; ---- buffer local variables

;; <$B%U%i%0N`(B>

;;(skk-deflocalvar skk-current-henkan-data
;;  '(;; global variables

;;    ;; $B%P%C%U%!%m!<%+%kJQ?t$N%G%#%U%)%k%HCM$r@_Dj$9$k$H!"$3$l$rD>@\=q49$($7$?(B
;;    ;; $B$H$-$KB>$N%P%C%U%!$+$i8+$($kCM$bJQ$o$C$F$7$^$&!#(Bglobal $B$J%U%i%0$O$3$l(B
;;    ;; $B$rMxMQ$7$F%G%#%U%)%k%HCM$rM?$($F$*$/!#(B

;;    ;; Emacs $B$r5/F08e4{$K(B skk-mode $B$r5/F0$7$?$3$H$r<($9(B
;;    (invoked . nil)

;;    ;; skk-isearch $B4X?t$r%3!<%k$9$k$?$a$N%U%i%0(B
;;    (isearch-message . nil)

;;    ;; $BJQ498uJd$r3NDj$7$?%+%&%s%H$rJ];}$9$kJQ?t(B
;;    (kakutei-count . 0)

;;    ;;$BF~NO%b!<%I$rI=$o$9%7%s%\%k(B
;;    (minibuffer-origin-mode . nil)

;;    ;; $B<-=qEPO?$7$?%+%&%s%H$rJ];}$9$kJQ?t(B
;;    (touroku-count . 0)

;;    ;; $B<-=q$r99?7$7$?2s?t(B
;;    (update-jisyo-count . 0)

;;    ;; buffer-local variables.

;;    ;; `skk-search-prog-list' $B$N8=:_$NCM$rJ]B8$9$k%j%9%H(B
;;    ;; (current-search-prog-list . nil)

;;    ;; $B%_%K%P%C%U%!$G8uJd$r<!!9$KI=<($7$F!"8uJd$,?T$-$?$3$H$r<($9(B
;;    ;; (exit-show-candidates . nil)

;;    ;; $B"'%b!<%I(B ($BJQ49Cf(B) $B$G$"$k$3$H$r<($9(B
;;    ;; (henkan-active . nil)

;;    ;; `skk-henkan-list' $B$N%j%9%H$N%$%s%G%/%9$G8=:_$N8uJd$r:9$9$b$N(B
;;    ;; (henkan-count . -1)

;;    ;; $BJQ49=*N;%]%$%s%H$r<($9%^!<%+!<(B
;;    ;; (henkan-end-point . nil)

;;    ;; $B%_%K%P%C%U%!$G<-=qEPO?$r9T$C$?$H$-$K$3$N%U%i%0$,N)$D(B
;;    ;; (henkan-in-minibuff-flag . nil)

;;    ;; $BJQ49$9$Y$-8+=P$78l(B
;;    ;; (henkan-key . nil)

;;    ;; $BJQ497k2L$N8uJd$N%j%9%H(B
;;    ;; (henkan-list . nil)


;;    ;; $B8=:_$NJQ49$NAw$j2>L>ItJ,(B
;;    ;; (henkan-okurigana . nil)

;;    ;; $B"&%b!<%I(B ($BJQ49BP>]$NJ8;zNs7hDj$N$?$a$N%b!<%I(B) $B$G$"$k$3$H$r<($9(B
;;    ;; (henkan-on . nil)

;;    ;; $BJQ493+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;; (henkan-start-point . nil)

;;    ;; $B3NDj$7$FNI$$8uJd$r8+$D$1$?>uBV$G$"$k$3$H$r;X$9(B
;;    ;; (kakutei-flag . nil)

;;    ;; $B$+$JJ8;z$N3+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;; (kana-start-point . nil)

;;    ;; $BF~NO%b!<%I$,%+%J%b!<%I$G$"$k$3$H$r<($9(B
;;    ;; (katakana . nil)

;;    ;; $B<-=q$NAw$jM-$j%(%s%H%j$N=*N;E@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;; (okuri-ari-max . nil)

;;    ;; $B<-=q$NAw$jM-$j%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;; (okuri-ari-min . nil)

;;    ;; $BJQ49$9$Y$-8l$NAw$j2>L>$NItJ,$N%W%l%U%#%C%/%9(B
;;    ;; (okuri-char . nil)

;;    ;; `skk-henkan-list' $B$N%$%s%G%/%9$G<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G(B
;;    ;; $B8!:w$7$?:G8e$N8uJd$r;X$9$b$N(B
;;    ;; (okuri-index-max . -1)

;;    ;; `skk-henkan-list' $B$N%$%s%G%/%9$G<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G(B
;;    ;; $B8!:w$7$?:G=i$N8uJd$r;X$9$b$N(B
;;    ;; (okuri-index-min . -1)

;;    ;; $B<-=q$NAw$j$J$7%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H(B
;;    ;; (okuri-nasi-min . nil)

;;    ;; $BAw$j2>L>ItJ,$,F~NOCf$G$"$k$3$H$r<($9(B
;;    ;;(okurigana . nil)

;;    ;; $BAw$j2>L>$N3+;O%]%$%s%H$r<($9%^!<%+!<(B
;;    ;; (okurigana-start-point . nil)

;;    ;; $BF~NO$9$k$+$J$r7hDj$9$k$?$a$N%W%l%U%#%C%/%9(B
;;    ;; (prefix . "")

;;    ;; $B$3$NJQ?t$KJ];}$5$l$k%]%$%s%H$,8=:_$N%]%$%s%H$H0[$J$k>l9g!"(B
;;    ;; `skk-with-point-move' $B$,;H$o$l$F$$$J$$%3%^%s%I$rF0:n$5$;$k$H(B
;;    ;; `skk-after-point-move' $B$,:nF0$9$k(B
;;    ;; (previous-point . nil)

;;    ;; `skk-insert' $B$b$7$/$O(B `skk-jisx0208-latin-insert' $B$GO"B3F~NO$7$?(B
;;    ;; $BJ8;z?t$rI=$o$9%+%&%s%?!<(B
;;    ;; (self-insert-non-undo-count . 1)))

(skk-deflocalvar skk-mode nil "\
Non-nil $B$G$"$l$P!"%+%l%s%H%P%C%U%!$G8=:_(B skk-mode $B$r5/F0$7$F$$$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-latin-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,(B ASCII $B%b!<%I$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-j-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,$+$J!&%+%J%b!<%I$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-katakana nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,%+%J%b!<%I$G$"$k$3$H$r<($9!#(B
\"(and (not skk-katakana) skk-j-mode))\" $B$,(B t $B$G$"$l$P!"$+$J%b!<%I$G$"$k$3$H$r(B
$B<($9!#(B")

(skk-deflocalvar skk-jisx0208-latin-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,A41Q%b!<%I$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-abbrev-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,(B SKK abbrev $B%b!<%I$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-okurigana nil
  "Non-nil $B$G$"$l$P!"Aw$j2>L>ItJ,$,F~NOCf$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-henkan-mode nil
  "$BJQ49%b!<%I$r<($9!#(B
`on' $B$G$"$l$P!""&%b!<%I!#(B
`active' $B$G$"$l$P!""'%b!<%I!#(B
`nil' $B$G$"$l$P!"3NDjF~NO%b!<%I!#(B")

(skk-deflocalvar skk-kakutei-flag nil
  "Non-nil $B$J$i3NDj$7$FNI$$8uJd$r8+$D$1$?>uBV$G$"$k$3$H$r;X$9!#(B
`skk-henkan', `skk-search-kakutei-jisyo-file', `skk-henkan-show-candidates',
`skk-henkan-in-minibuff' $B$H(B `skk-kakutei-save-and-init-variables' $B$GJQ99!"(B
$B;2>H$5$l$k!#(B")

(skk-deflocalvar skk-exit-show-candidates nil
  "$B%_%K%P%C%U%!$G8uJd$r<!!9$KI=<($7$F!"8uJd$,?T$-$?$H$-$K(B non-nil $B$H$J$k!#(B
$B$=$NCM$O%j%9%H$G!"(Bcar $B$K(B `skk-henkan-show-candidate' $B4X?t$G(B while $B%k!<%W$r(B
$B2s$C$?2s?t$r<($90l;~JQ?t(B loop $B$NCM$r!"(Bcdr $BIt$K:G8e$K%_%K%P%C%U%!$KI=<($7$?(B
1 $B$DA0$N8uJd72$N:G8e$NMWAG$r;X$9%$%s%G%/%9$,BeF~$5$l$k!#(B
`skk-henkan-show-candidates', `skk-henkan-in-minibuff' $B$H(B
`skk-kakutei-save-and-init-variables' $B$GJQ99!";2>H$5$l$k!#(B")

 ;; <$B%-!<%^%C%W4XO"(B>
(skk-deflocalvar skk-current-rule-tree nil
  "$B%m!<%^;z(B -> $B$+$JJQ49$N>uBVA+0\5,B'$rI=$o$9%D%j!<$N8=;~E@$N>uBV!#(B
$B%m!<%^;zF~NO$N=i4|$G$O(B `skk-rule-tree' $B$HF10l$N>uBV$G!"J8;zF~NO$,?J$`$K(B
$B$D$l!"LZ$r$?$I$C$F$f$/>uBV$NA+0\$rI=$9!#(B")

;; <$B<-=q4XO"$NJQ?t(B>
(skk-deflocalvar skk-okuri-ari-min nil
  "SKK $B<-=q$NAw$jM-$j%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H!#(B")

(skk-deflocalvar skk-okuri-ari-max nil
  "SKK $B<-=q$NAw$jM-$j%(%s%H%j$N=*N;E@$r<($9%P%C%U%!%]%$%s%H!#(B
skk-jisyo $B$N%P%C%U%!$G$O<-=q$N99?7$NI,MW$,$"$k$?$a$K%^!<%+!<$,BeF~$5$l$k!#(B")

(skk-deflocalvar skk-okuri-nasi-min nil
  "SKK $B<-=q$NAw$j$J$7%(%s%H%j$N3+;OE@$r<($9%P%C%U%!%]%$%s%H!#(B
skk-jisyo $B$N%P%C%U%!$G$O<-=q$N99?7$NI,MW$,$"$k$?$a$K%^!<%+!<$,BeF~$5$l$k!#(B")

;; <$B$=$NB>(B>
(skk-deflocalvar skk-mode-line nil
  "SKK $B$N%b!<%I$r<($9%b!<%I%i%$%s$NJ8;zNs!#(B
`skk-mode-string', `skk-hiragana-mode-string', `skk-katakana-mode-string',
 `skk-jisx0208-latin-mode-string' $B$N$$$:$l$+$,BeF~$5$l$k!#(B")

(skk-deflocalvar skk-previous-point nil
  "`skk-with-point-move' $B4XO"JQ?t!#(B
$B$3$NJQ?t$KJ];}$5$l$k%]%$%s%H$,8=:_$N%]%$%s%H$H0[$J$k>l9g!"(B`skk-with-point-move'
$B$,;H$o$l$F$$$J$$%3%^%s%I$rF0:n$5$;$k$H!"(B`skk-after-point-move' $B$,:nF0$9$k!#(B")

(skk-deflocalvar skk-prefix ""
  "$BF~NO$9$k$+$J$r7hDj$9$k$?$a$N%W%l%U%#%C%/%9!#(B")

(skk-deflocalvar skk-henkan-start-point nil
  "$BJQ493+;O%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-henkan-end-point nil
  "$BJQ49=*N;%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-kana-start-point nil
  "$B$+$JJ8;z$N3+;O%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-okurigana-start-point nil
  "$BAw$j2>L>$N3+;O%]%$%s%H$r<($9%^!<%+!<!#(B")

(skk-deflocalvar skk-henkan-key nil
  "$BJQ49$9$Y$-8+=P$78l!#(B
$BNc$($P!"(B\"$B"&$+$J(B\" $B$rJQ49$9$l$P!"(B`skk-henkan-key' $B$K$O(B \"$B$+$J(B\" $B$,BeF~$5$l$k!#(B
\"$B"&$o$i(B*$B$&(B\" $B$N$h$&$JAw$j$"$j$NJQ49$N>l9g$K$O!"(B\"$B$o$i(Bu\" $B$N$h$&$K!"4A;zItJ,$N(B
$BFI$_$,$J(B + $BAw$j2>L>$N:G=i$NJ8;z$N%m!<%^;z$N%W%l%U%#%C%/%9$,BeF~$5$l$k!#(B")

(skk-deflocalvar skk-okuri-char nil
  "$BJQ49$9$Y$-8l$NAw$j2>L>$NItJ,$N%W%l%U%#%C%/%9!#(B
$BNc$($P!"(B\"$B$*$/(B*$B$j(B\" $B$rJQ49$9$k$H$-$O!"(B`skk-okuri-char' $B$O(B \"r\"$B!#(B
`skk-okuri-char' $B$,(B non-nil $B$G$"$l$P!"Aw$j$"$j$NJQ49$G$"$k$3$H$r<($9!#(B")

(skk-deflocalvar skk-henkan-okurigana nil
  "$B8=:_$NJQ49$NAw$j2>L>ItJ,!#(B
$BNc$($P!"(B\"$B"&$&$^$l(B*$B$k(B\" $B$rJQ49$9$l$P!"(B`skk-henkan-okurigana' $B$K$O(B \"$B$k(B\" $B$,BeF~(B
$B$5$l$k!#(B")

(skk-deflocalvar skk-last-kakutei-henkan-key nil
  "$B3NDj<-=q$K$h$j:G8e$K3NDj$7$?$H$-$N8+=P$78l!#(B
$B3NDj<-=q$K$h$k3NDj$ND>8e$K(B x $B%-!<$r2!$9$H3NDj$,%"%s%I%%$5$l$F!"3NDjA0$N>uBV$G(B
$B$3$N8+=P$78l$,%+%l%s%H%P%C%U%!$KA^F~$5$l$k!#(B")

(skk-deflocalvar skk-henkan-list nil
  "$BJQ497k2L$N8uJd$N%j%9%H!#(B
$BNc$($P!"(B\"$B"&$J(B*$B$/(B\" $B$H$$$&JQ49$9$l$P!"(B`skk-henkan-list' $B$O(B
(\"$BLD(B\" \"$B5c(B\" \"$BL5(B\" \"$BK4(B\") $B$N$h$&$K$J$k!#(B")

(skk-deflocalvar skk-henkan-count -1
  "`skk-henkan-list' $B$N%j%9%H$N%$%s%G%/%9$G8=:_$N8uJd$r:9$9$b$N!#(B")

(skk-deflocalvar skk-self-insert-non-undo-count 1
  "$BO"B3F~NO$7$?J8;z?t$rI=$o$9%+%&%s%?!<!#(B
`skk-insert' $B$b$7$/$O(B `skk-jisx0208-latin-insert' $B$G%+%&%s%H$5$l$k!#(B
Emacs $B$N%*%j%8%J%k$NF0:n$G$O!"(B`self-insert-command' $B$K%P%$%s%I$5$l$?%-!<F~NO$O(B
$BO"B3(B 20 $B2s$^$G$,(B 1 $B$D$N%"%s%I%%$NBP>]$H$J$k!#$3$NF0:n$r%(%_%e%l!<%H$9$k$?$a$N(B
$B%+%&%s%?!<!#$3$N%+%&%s%?!<$,!"(B20 $B0J2<$G$"$k$H$-$O!"F~NO$N$?$S$K(B
`cancel-undo-boundary' $B$,%3!<%k$5$l$k!#(B")

(skk-deflocalvar skk-current-search-prog-list nil
  "`skk-search-prog-list' $B$N8=:_$NCM$rJ]B8$9$k%j%9%H!#(B
$B:G=i$NJQ49;~$O(B `skk-search-prog-list' $B$NA4$F$NCM$rJ];}$7!"JQ49$r7+$jJV$9$?$S$K(B
1 $B$D$:$DC;$/$J$C$F$f$/!#(B")

;; for skk-undo-kakutei
(skk-deflocalvar skk-last-henkan-data nil
  "$B:G8e$K9T$C$?JQ49$K4X$9$k%G!<%?$NO"A[%j%9%H!#(B
$B%G%#%U%)%k%H$N%-!<$O!"(B`henkan-key', `henkan-okurigana', `okuri-char',
`henkan-list' $B$N3F%7%s%\%k!#(B
 (skk-num $B$r(B require $B$7$F$$$k$H$-$O!"(Bnum-list $B$,DI2C$5$l$k(B)$B!#(B")

(skk-deflocalvar skk-henkan-overlay nil
  "$B8uJd$rI=<($9$k$H$-$K;HMQ$9$k(B Overlay$B!#(B")

(skk-deflocalvar skk-henkan-in-minibuff-flag nil
  "$B%_%K%P%C%U%!$G<-=qEPO?$r9T$C$?$H$-$K$3$N%U%i%0$,N)$D!#(B
skk-remove-common $B$G;2>H$5$l$k!#(B")

(skk-deflocalvar skk-okuri-index-min -1
  "`skk-henkan-list'$B$N%$%s%G%/%9$rA^$9%]%$%s%?$N$R$H$D!#(B
$B<+F0Aw$j=hM}!&%5JQ8!:w$G8!:w$7$?:G=i$N8uJd$r;X$9!#(B")

(skk-deflocalvar skk-okuri-index-max -1
  "skk-henkan-list $B$N%$%s%G%/%9$rA^$9%]%$%s%?$N$R$H$D!#(B
$B<+F0Aw$j=hM}!"$b$7$/$O%5JQ8!:w$G8!:w$7$?:G8e$N8uJd$r;X$9!#(B")

(skk-deflocalvar skk-last-buffer-undo-list nil
  "$B"&%b!<%I$KF~$kD>A0$N(B buffer-undo-list $B$rB`Hr$7$F$*$/JQ?t!#(B")

(skk-deflocalvar skk-last-buffer-modified nil
  "$B"&%b!<%I$KF~$kD>A0$N%P%C%U%!JQ99%U%i%0$rB`Hr$7$F$*$/JQ?t!#(B")

;;; -- SKK-COMP.EL related internal variables
;; ---- buffer local variables
;; $B6uJ8;zNs$KBP$7$F(B skk-comp-do $B$r8F$V$3$H$b$"$j$&$k$N$G!"(B"" $B$r(B nil $B$G$OBe(B
;; $BMQ$G$-$J$$!#(B
(skk-deflocalvar skk-comp-key ""
  "$BJd40$9$Y$-8+=P$78l!#(B
`skk-dabbrev-like-completion' $B$,(B non-nil $B$N>l9g$O!">o$K:G8e$KJd40$7$?8+=P$78l$,(B
$BBeF~$5$l$k!#(B")
;; $B<-=qEPO?;~%_%K%P%C%U%!$GJd40$7$?>l9g!"85$N%P%C%U%!$KLa$C$?$H$-$K(B
;; skk-comp-key $B$NCM$,GK2u$5$l$F$$$J$$J}$,%Y%?!<!#(B

(skk-deflocalvar skk-comp-stack nil
  "$BJd40$7$?8l$rJ]B8$7$F$*$/%9%?%C%/!#(B")

(skk-deflocalvar skk-comp-depth 0
  "$BJd40$7$?8l$r(B skk-comp-stack $B$+$i<h$j=P$90LCV!#(B")

(skk-deflocalvar skk-comp-kakutei-midasi-list nil
  "$B3NDjMzNr$+$iF@$i$l$?8+=P$78l$N%j%9%H!#(B")

;;; -- SKK-CURSOR.EL related internal variables

;;; -- SKK-GADGET.EL related internal variables

;;; SKK-ISEARCH.EL related internal constants and variables.
(defconst skk-isearch-mode-canonical-alist
  '((hiragana . 0) (katakana . 1) (jisx0208-latin . 2) (latin . 3))
  "Alist of (SYMBOL . NUMBER).
The SYMBOL is canonical skk mode, and NUMBER is its numerical representation.")

(defconst skk-isearch-mode-alias-alist
  '((hirakana . hiragana) (kana . hiragana) (eiji . jisx0208-latin)
    (ascii . latin))
  "Alist of (ALIAS . CANONICAL).
The both ALIAS and CANONICAL should be symbol.
ALIAS can be used as an alias of CANONICAL.
CANONICAL should be found in `skk-isearch-mode-canonical-alist'. ")

(defconst skk-isearch-breakable-character-p-function
  (static-cond
   ((fboundp 'char-category-set)
    (function (lambda (char)
		;; see emacs/lisp/fill.el how the category `|' is
		;; treated.
		(aref (char-category-set char) ?|))))
   ((boundp 'word-across-newline)
    (function (lambda (char)
		;; (let ((lc (char-leading-char char)))
		;;   (or (= lc lc-jp) (= lc lc-cn)))
		(string-match word-across-newline
			      (char-to-string char)))))
   (t (error "No appropriate function as: %s"
	     'skk-isearch-breakable-character-p-function)))
  "Function to test if we can insert a newline around CHAR when filling.")

(defconst skk-isearch-working-buffer " *skk-isearch*"
  "Work buffer for skk isearch.")

(defvar skk-isearch-mode nil
  "Current search mode.
0 means hira kana search.
1 means kana search.
2 means zenkaku eiji (i.e. JIS X0208 alphabet) search.
3 means ascii search.")

(defvar skk-isearch-incomplete-message ""
  "Incomplete isearch message")

(defvar skk-isearch-mode-map nil
  "Keymap for skk isearch mode.
This map should be derived from isearch-mode-map.")

(defvar skk-isearch-overriding-local-map
  (static-cond
   ((eq skk-emacs-type 'xemacs)
    (cond
     ((string-lessp "21.2  (beta2)" emacs-version)
      'overriding-local-map)
     (t
      'overriding-terminal-local-map)))
   ;; for Mule/GNU Emacs.
   ((string-lessp "19.29" emacs-version)
    ;; GNU Emacs version 19.29 or later uses this in isearch.el.
    'overriding-terminal-local-map)
   ;; GNU Emacs version 19.28 or earlier uses this in isearch.el.
   (t
    'overriding-local-map))
  "Variable holding overrinding local map used in isearch-mode.")

(defvar skk-isearch-last-mode-string "")
(defvar skk-isearch-last-mode-regexp "")

;;;###autoload
(defvar skk-isearch-switch nil)
(defvar skk-isearch-state nil)
(defvar skk-isearch-in-editing nil)
(defvar skk-isearch-current-buffer nil)

;;; -- SKK-JISX0201.EL related internal constants and variables.
(defvar skk-jisx0201-base-rule-tree nil)
(defvar skk-jisx0201-roman-rule-tree nil)
(defvar skk-jisx0201-orig-rule-tree nil)
(skk-deflocalvar skk-jisx0201-roman nil)

(skk-deflocalvar skk-jisx0201-mode nil
  "Non-nil $B$G$"$l$P!"F~NO%b!<%I$,(B JISX0201 $B%b!<%I$G$"$k$3$H$r<($9!#(B")

;;; -- SKK-KCODE.EL related internal constants and variables.
(defconst skk-code-n1-min 161)
(defconst skk-code-n1-max (if (featurep 'jisx0213) 254 244))
(defconst skk-code-n2-min 161)
(defconst skk-code-n2-max 254)
(defconst skk-code-null 128)
(defconst skk-kcode-charset-list
  (static-if (memq skk-emacs-type '(xemacs mule5 mule4 mule3))
      (mapcar '(lambda (x) (list (symbol-name x))) (charset-list))))
(defvar skk-input-by-code-or-menu-jump-default skk-code-n1-min)

;;; SKK-LOOK.EL related internal constant and variable.
(defvar skk-look-completion-words nil)

;;; SKK-NUM.EL related internal constants and variables
(defconst skk-num-alist-type1
  '((?0 . "$B#0(B") (?1 . "$B#1(B") (?2 . "$B#2(B") (?3 . "$B#3(B")
    (?4 . "$B#4(B") (?5 . "$B#5(B") (?6 . "$B#6(B") (?7 . "$B#7(B")
    (?8 . "$B#8(B") (?9 . "$B#9(B")
    (?. . "$B!%(B")	; $B>.?tE@!#(B(?. . ".") $B$NJ}$,NI$$?M$b$$$k$+$b(B...$B!#(B
    (?  . ""))
  "ascii $B?t;z$N(B char type $B$HA43Q?t;z$N(B string type $B$NO"A[%j%9%H!#(B
\"1995\" -> \"$B#1#9#9#5(B\" $B$N$h$&$JJ8;zNs$NJQ49$r9T$&:]$KMxMQ$9$k!#(B")

(defconst skk-num-alist-type2
  '((?0 . "$B!;(B") (?1 . "$B0l(B") (?2 . "$BFs(B") (?3 . "$B;0(B")
    (?4 . "$B;M(B") (?5 . "$B8^(B") (?6 . "$BO;(B") (?7 . "$B<7(B")
    (?8 . "$BH,(B") (?9 . "$B6e(B") (?\  . ""))
  "ascii $B?t;z$N(B char type $B$H4A?t;z$N(B string type $B$NO"A[%j%9%H!#(B
\"1995\" -> \"$B0l6e6e8^(B\" $B$N$h$&$JJ8;zNs$NJQ49$r9T$&:]$KMxMQ$9$k!#(B")

(defconst skk-num-alist-type3
  (append
   '((ju . "$B==(B") (hyaku . "$BI4(B") (sen . "$B@i(B")
     (man . "$BK|(B") (oku . "$B2/(B") (cho . "$BC{(B") (kei . "$B5~(B"))
   skk-num-alist-type2)
  "$B?t;z$N4A;zI=5-$rI=$9O"A[%j%9%H!#(B
\"1995\" -> \"$B@i6eI46e==8^(B\" $B$N$h$&$JJ8;zNs$NJQ49$r9T$&:]$KMxMQ$9$k!#(B")

(defconst skk-num-alist-type5
  '((ju . "$B=&(B") (hyaku . "$BI4(B") (sen . "$Bot(B")
    (man . "$Bh_(B") (oku . "$B2/(B") (cho . "$BC{(B") (kei . "$B5~(B")
    (?0 . "$BNm(B") (?1 . "$B0m(B") (?2 . "$BFu(B") (?3 . "$B;2(B")
    (?4 . "$B;M(B") (?5 . "$B8`(B") (?6 . "$BO;(B") (?7 . "$B<7(B")
    (?8 . "$BH,(B") (?9 . "$B6e(B") (?\  . ""))
  "$B?t;z$N4A;zI=5-$rI=$9O"A[%j%9%H!#(B
\"1995\" -> \"$B0mot6eI46e=&8`(B\" $B$N$h$&$JJ8;zNs$NJQ49$r9T$&:]$KMxMQ$9$k!#(B")

(skk-deflocalvar skk-num-list nil
  "skk-henkan-key $B$NCf$K4^$^$l$k?t;z$rI=$9J8;zNs$N%j%9%H!#(B
$BNc$($P!"(B\"$B"&$X$$$;$$(B7$B$M$s(B10$B$,$D(B\" $B$NJQ49$r9T$&$H$-!"(Bskk-henkan-key $B$O(B
\"$B$X$$$;$$(B7$B$M$s(B10$B$,$D(B\" $B$G$"$j!"(Bskk-num-list $B$O(B \(\"7\" \"10\"\) $B$H$J$k!#(B
\(buffer local\)")

(defvar skk-num-recompute-key nil
  "#4 $B%?%$%W$N%-!<$K$h$j?tCM$N:F7W;;$r9T$C$?$H$-$N8!:w%-!<!#(B")

;;; SKK-SERVER.EL related internal constants and variables.
(defconst skk-network-open-status 'open)
(defconst skkserv-working-buffer " *skkserv*")
(defvar skkserv-process nil)

;;; SKK-VIPER.EL related internal variables and constants.
(defvar skk-viper-saved-cursor-color
  (when (and (featurep 'viper)
	     (boundp 'viper-insert-state-cursor-color))
    (symbol-value 'viper-insert-state-cursor-color)))
(make-variable-buffer-local 'viper-insert-state-cursor-color)

(defconst skk-viper-use-vip-prefix
  (not (fboundp 'viper-normalize-minor-mode-map-alist)))

(defconst skk-viper-normalize-map-function
  (if skk-viper-use-vip-prefix
      'vip-normalize-minor-mode-map-alist
    'viper-normalize-minor-mode-map-alist)
  "Viper $B$,(B minor-mode-map-alist $B$rD4@0$9$k$?$a$N4X?t!#(B")

;;;; SKK-LOOKUP related user variables.
(defcustom skk-lookup-search-agents nil
  "*$B8!:w%(!<%8%'%s%H$N@_Dj$N%j%9%H!#(B
$B%j%9%H$N3FMWAG$O<!$N7A<0$r<h$k(B:

  \(CLASS LOCATION [KEY1 VALUE1 \[KEY2 VALUE2 \[...\]\]\]\)

CLASS $B$K$O!"%(!<%8%'%s%H$N<oN`$r%7%s%\%k$G;XDj$9$k!#(B
LOCATION $B$K$O!"%(!<%8%'%s%H$N=j:_$rJ8;zNs$G;XDj$9$k!#(B
KEY $B5Z$S(B VALUE $B$O>JN,2DG=$G!"%(!<%8%'%s%H$KBP$9$k%*%W%7%g%s$r;XDj$9$k!#(B

$BNc(B: (setq skk-lookup-search-agents
          '((ndtp \"dserver\" :port 2010)
            (ndeb \"/cdrom\" :enable (\"EIWA\")))))"
  :type '(repeat (sexp :tag "Agent"))	; type $B$O$A$g$C$H$d$d$3$7$9$.!&!&(B
  :group 'skk-lookup)

(defcustom skk-lookup-option-alist
  '(;; "[spla -> splat]"
    ("ispell" exact nil nil (not skk-okuri-char) ("-> \\([^ ]+\\)]$" . 1)
     nil nil)
    ;; what's this?
    ("jedict" exact nil nil (not skk-okuri-char) nil nil nil)
    ;; $BCN7CB"(B
    ;; `$B"'#I#M#F!N(BInternational Monetary Fund$B!?(BInternational
    ;;            Metalworkers Federation$B!O(B'
    ;; `$B#I#M#F!J9q:]DL2_4p6b!K!Z(BInternational Monetary Fund$B![(B'
    ("CHIEZO" exact exact prefix t
     ("$B!J(B\\(.+\\)$B!K(B\\|$B!Z(B\\(.+\\)$B![(B$\\|$B!N(B\\(.+\\)$B!O(B$\\|^\\([^$B!J!Z!N!O![!K(B]+\\)$"
      .
      (cond ((match-beginning 1) 1)
	    ((match-beginning 2) 2)
	    ((match-beginning 3) 3)
	    ((match-beginning 4) 4)))
     "$B!?(B\\|$B!"(B\\|, " nil)
    ;; $B!V<-!&E5!&HW!W(B
    ;; `$B$"$+#3(B $B^@(B", "ethanol'
    ("CHUJITEN" exact exact prefix t ("[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" . 1) nil nil)
    ;; `($BHiIf$J$I$N(B)$B$"$+(B <grime>", "$B!T1Q!U(B ($B%Q%$%W$J$I$N(B)$B$"$+(B <fur>'
    ("COLLOC" exact exact prefix t ("\\([^ $B!T!U(B]+\\) <[a-z]+>$" . 1) nil nil)
    ;; $B%8!<%K%"%91QOB(B, $B%8!<%K%"%91QOB!&OB1Q<-E5(B
    ;; `$B$"$+(B[$B^@(B]'
    ;; `$B$$$l$+$((B[$BF~$lBX$((B,$BF~$l49$((B]'
    ("GENIUS" exact exact prefix t
     ;;("\\[\\(.+\\)\\]$" . 1) ;;can I use `$' for GENIUS?
     ("\\[\\(.+\\)\\]" . 1)
     "," nil)
    ;; Super$BE}9g<-=q(B99 Disk1, 2/$B8=BeMQ8l$N4pACCN<1(B
    ;; `$B"!<k!&3t!&<l!&<n!L;w$?$b$N4A;z!M(B' ; `$B!&(B' $B$,6h@Z$jJ8;z$G$"$k$H$-$H(B
    ;;  $B$=$&$G$J$$$H$-$,$"$k$J$!(B...$B!#(B
    ;; `$B"!@V%o%$%s!&%V!<%`!L7r9/LdBj!M(B'
    ("GN99EP01" exact exact prefix t ("^$B"!(B\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" . 1) nil nil)
    ("GN99EP02" exact exact prefix t ("^$B"!(B\\([^$B!L!M(B]+\\)$B!L(B.+$B!M(B$" . 1) nil nil)
    ;; $B4dGH9q8l<-E5(B
    ;; `$B$7$?$$!Z;`BN!&;SBN![(B'
    ;; `$B$7$?$$!Z;YBb![!Z;^Bb![(B'
    ;; `$B$"$$!Z0&![(B'
    ;; `$B$"$$(B($B$"$p(B)$B!ZMu![(B'
    ;; `$B$"$$(B<gaiji=za52a>$B0%(B<gaiji=za52b>'
    ;; `$B$@$7!Z=P$7![!Z=P$7!&!R=P=A!S![!Z!P;3<V!Q![(B'
    ;; `$B$U$&$-$j!ZIu@Z(B($B$j(B)$B![(B'
    ("IWAKOKU" exact exact prefix t
     ;; cannot use `$' for this.
     ("$B!Z(B\\(.+\\)$B![(B" . 1)
     "$B![!Z(B\\|$B!&(B" "[$B!R!S!P!Q(B()]")
    ;; "$B9$(B", "$B@V(B"
    ("KANWA" exact exact prefix t nil nil nil)
    ;; KOUJIEN: $B9-<-1q(B $BBh(B4$BHG(B($B4dGH(B,EPWING) $B%^%k%A%a%G%#%"HG(B
    ;; `$B$"$$!Z9g$$!&2q$$![%"%R(B' ; $B$3$l$K$O(B `$B![(B$' $B$r;H$($J$$!#(B
    ;; `$B$"$$!Z4V![%"%R(B'
    ;; `$B%&%#!Z(Boui $B%U%i%s%9![(B'
    ;; `$B%=!Z(Bsol $B%$%?%j%"![(B'
    ;; `$B%"%j%9%H%F%l%9!>$7$e$.!Z!=<g5A![(B'
    ;; `$B%"!<%H%^%s!Z(B_tman $B[p![(B'; $BL$BP1~!#30;z$r4^$`8uJd!#(B_ $B$O30;z(B
    ("KOUJIEN" exact exact prefix t
     ("^\\([^$B!Z![(B]+\\)$B!>(B[$B!<$!(B-$B$s(B]+$B!Z!=(B\\([^$B!Z![(B]+\\)$B![(B$\\|\
$B!Z(B\\([a-zA-Z]+\\) [$B!<%!(B-$B%s(B]+$B![(B$\\|$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" .
      (cond ((match-beginning 2) '(1 2))
	    ((match-beginning 3) 3)
	    ((match-beginning 4) 4)))
     "$B!&(B"
     ;;"$B!>(B[$B!<$!(B-$B$s(B]+$B!Z!=(B\\|$B![(B$"
     nil)
    ;; KOJIEN: $B9-<-1qBh(B5$BHG(B($B4dGH(B,EPWING)
    ;; `$B$G$s$7!>%V%C%/!ZEE;R!=![(B'
    ("KOJIEN" exact exact prefix t
     ("^\\([^$B!Z![(B]+\\)$B!>(B[$B!<$!(B-$B$s(B]+$B!Z!=(B\\([^$B!Z![(B]+\\)$B![(B$\\|\
$B!Z(B\\([a-zA-Z]+\\) [$B!<%!(B-$B%s(B]+$B![(B$\\|$B!Z(B\\([^$B!Z![(B]+\\)$B![(B\\|\
^[$B!<$!(B-$B$s(B]+$B!>(B\\([$B!<%!(B-$B%s(B]+\\)$B!Z(B\\([^$B!Z![(B]+\\)$B!=![(B$" .
      (cond ((match-beginning 2) '(1 2))
	    ((match-beginning 3) 3)
	    ((match-beginning 4) 4)
	    ((match-beginning 5) '(6 5))))
     "$B!&(B"
     ;;"$B!>(B[$B!<$!(B-$B$s(B]+$B!Z!=(B\\|$B![(B$"
     nil)
    ;; KOKUGO: $B;0>JF2(B $BF|K\8l<-E5!J8=Be9q8l!"30Mh8l!K(B
    ;; `$B!R(B' $B$O!"EvMQ4A;zI=$K$J$$4A;z$G!"(B`$B!T(B' $B$O!"EvMQ4A;zI=$K$O$"$k$,!"$=$N2;!"(B
    ;; $B71$,EvMQ4A;zI=$N2;71I=$K$J$$4A;z!#(B
    ("KOKUGO" exact exact prefix t ("$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" . 1) "$B!&(B" "[$B!T!R(B]")
    ;; $B!V<-!&E5!&HW!WImB0$N%^%$%Z%G%#%"(B
    ;;`$BBgOB74;3(B($B;T(B)'
    ;;`$B%o%7%s%H%s(B(George Washington)'
    ;;`$B%o%7%s%H%s(B($B=#(B)'
    ;;`$B%o%7%s%H%s(B Washington'
    ;;`$B%"%$%s%7%e%?%$%s(B(Albert Einstein)'
    ;;`$B9aNI='(B($BD.(B)'
    ;;`$B%+%i%9(B ($B1((B)'
    ;;`$B%+%i%9(B(Maria Callas)'
    ("MYPAEDIA" exact exact prefix t
     ("\\([^ ]+\\)(.+)$\\|.+ (\\([^ ]+\\))$\\|^\\([^ ()]+\\)$" .
      (cond ((match-beginning 1) 1)
	    ((match-beginning 2) 2)
	    ((match-beginning 3) 3)))
     nil nil)
    ;;  mypaedia-fpw $B$+$i@8@.$7$?(B PC Success $BHG%^%$%Z%G%#%"(B (FreePWING $B<-=q(B)
    ;; `$BBgOB74;3(B [$B$d$^$H$3$*$j$d$^(B] ($B;T(B)'
    ;; `$B%"%$%s%7%e%?%$%s(B (Albert Einstein)'
    ;; `$B%o%7%s%H%s(B (Washington) ($B=#(B)'
    ;; `$B%o%7%s%H%s(B (Washington)'
    ;; `$B%o%7%s%H%s(B (George Washington)'
    ;; `$B9aNI='(B [$B$+$i$9(B] ($BD.(B)'
    ;; `$B%+%i%9(B ($B1((B) [$B%+%i%9(B]'
    ;; `$B%+%i%9(B (Maria Callas)'
    ;;("MYPAEDIA" exact exact prefix t
    ;; ("^\\([^ ]+\\) \\[.+\\] (.+)$\\|^[^ ]+ (\\(.+\\)) \\[.+\\]$\\|\
    ;;   ^\\([^][() ]+\\)\\( .+\\)?$" .
    ;;  (cond ((match-beginning 1) 1)
    ;;        ((match-beginning 2) 2)
    ;;        ((match-beginning 3) 3)))
    ;; nil nil)
    ;;
    ;; $B%K%e!<%"%s%+!<1QOB(B
    ;; "$B$"$+#2(B $B9$(B"
    ("NEWANC" exact exact prefix t ("[$B#0(B-$B#9(B]* *\\([^ ]+\\)$" . 1) nil nil)
    ;; what's this?
    ;; `$B!!$"$+(B <scud$B#2(B>',
    ;; `$B!!!V$"$+!W(B <rust>'
    ("PLUS" exact exact prefix t ("^$B!!(B\\(.+\\) <[a-z$B#0(B-$B#9(B]+>$" . 1) nil nil))
  "*$B<-=qKh$N8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: `lookup-dictionary-name' $B$,JV$9J8;zNs(B ($B<-=q<oJL$rI=$o$9(B)$B!#(B
  1th: $BAw$j$J$7JQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(B
  2th: $BAw$j$"$jJQ49$G!"$+$D(B `skk-process-okuri-early' $B%*%W%7%g%s$r;XDj$7$F(B
       $B$$$J$$$H$-(B ($BAw$j2>L>7hDj$N8e$K8!:w$r3+;O$9$k$N$G!"Aw$j2>L>$,FCDj$G$-$k(B)
       $B$N(B search method $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(Bnil $B$r;XDj$9$k$H!"(B
       $BAw$j$"$jJQ49$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  3th: $BAw$j$"$jJQ49$G!"$+$D(B `skk-process-okuri-early' $B$G$"$k$H$-(B ($BAw$j2>L>(B
       $B7hDj$NA0$K8!:w$r3+;O$7$F$*$j!"Aw$j2>L>$,FCDj$G$-$J$$$N$G!"Aw$j2>L>$N$+$J(B
       prefix $B$r=|$$$?ItJ,$r8!:w%-!<$H$7$F(B lookup $B$KEO$7$F$$$k(B) $B$N(B search
       method $B$r<($9(B $B%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(Bnil $B$r;XDj$9$k$HAw$j$"$jJQ49(B
       $B$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  4th: S $B<0!#$3$N(B S $B<0$rI>2A$7$F(B nil $B$K$J$k$H$-$O8!:w$7$J$$!#$"$k0lDj$N>r7o$rK~(B
       $B$7$?>l9g$K8!:w$7$J$$$h$&$K;XDj$G$-$k!#(B
  5th: `lookup-entry-heading' $B$,JV$9(B heading $B$+$i8uJd$H$7$F=PNO$9$kJ8;zNs$r@Z$j(B
       $B=P$9$?$a$N(B regexp $B;XDj5Z$S@Z$j=P$7%*%W%7%g%s!#(B
       car $B$K(B regexp $B$r<($9J8;zNs!"(Bcdr $B$K(B `match-string' $B$KEO$9(B count $B$r;XDj(B
       $B$9$k(B (5th $B$KJ8;zNs$@$1$r;XDj$7$?>l9g$O(B `match-string' $B$K$O(B 1 $B$,(B
       $BEO$5$l$k(B)$B!#(B
       cdr $BIt$K(B S $B<0$r;XDj$9$k$3$H$b2DG=!#2<5-$N$h$&$K(B cond $B<0$G>r7oH=Dj$9$l$P(B
       $BJ#?t$N(B regexp $B$r(B or $B;XDj$9$k$3$H$,2DG=!#(B

          (cond ((match-beginning 1) 1)
                ((match-beginning 2) 2)
	        ((match-beginning 3) 3)
                ((match-beginning 4) 4))

       cdr $BIt$NI>2A7k2L$,?t;z$N%j%9%H$K$J$k$H$-$O!"$=$N?t;z$r=g$K(B match-string
       $B$KEO$7$FJ8;zNs$r@Z$j=P$7!"$=$l$iO"7k$7$?J8;zNs$r8uJd$H$7$FJV$9!#Nc$($P!"(B

          (cond ((match-beginning 5) '(6 5)))

       $B$H;XDj$9$k$H!"(B(match-beginning 5) $B$,(B non-nil $B$K$J$C$?>l9g!"(B
       (match-string 6) $B$H(B (match-string 5) $B$r$=$N=g$KO"7k$7$?J8;zNs$r8uJd$H$7(B
       $B$F=PNO$9$k!#(B
       $B@Z$j=P$5$:$KJ8;zNsA4BN$rBP>]$K$9$k$H$-$O!"(B5th $B$K(B nil $B$r;XDj$9$k!#(B
  6th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B
       regexp$B!#(B
       $BJ#?t$N8uJd$,F10l(B heading $B$NCf$K=PNO$5$l$J$$$H$-$O!"(Bnil $B$r;XDj$9$k!#(B
  7th: $B@Z$j=P$5$l$?J8;zNs$+$iFCDj$NJ8;zNs$r<h$j=|$/>l9g$K;XDj$9$k(B regexp$B!#(B
       $B<-=q$N=PNO$,<-=qFCM-$N5-9fJ8;z$r4^$`>l9g$K;XDj$9$k!#(B

$B8=:_BP1~$7$F$$$k<-=qL>$O(B \"ispell\", \"jedict\", \"CHIEZO\", \"CHUJITEN\",
\"COLLOC\", \"GENIUS\", \"GN99EP01\", \"GN99EP02\", \"IWAKOKU\", \"KANWA\",
\"KOUJIEN\", \"KOJIEN\", \"MYPAEDIA\" \"NEWANC\" $B5Z$S(B \"PLUS\"$B!#(B
`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 (skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact)"
  ;; for checking.
  ;; (pp (mapcar (lambda (e)(cons (car e) (length e)))
  ;;	skk-lookup-option-alist))
  :type '(repeat
	  (list (string :tag "Dictionary name")
		(choice :tag "Search method for okuri nasi"
			(const exact) (const prefix)
			(const suffix) (const substring)
			(const keyword) (const text)
			(const nil))
		(choice
		 :tag "Search method for okuri ari (not process okuri early)"
		 (const exact) (const prefix)
		 (const suffix) (const substring)
		 (const keyword) (const text)
		 (const nil))
		(choice
		 :tag "Search method for okuri ari (process okuri early)"
		 (const exact) (const prefix)
		 (const suffix) (const substring)
		 (const keyword) (const text)
		 (const nil))
		(sexp :tag "S expression to search")
		(choice :tag "Regexp to substring candidate from heading"
			(cons regexp sexp) (const nil))
		(choice :tag "Regexp to split candidates"
			regexp (const nil))
		(choice :tag "Regexp to remove a string from candidates"
			regexp (const nil))))
  :group 'skk-lookup)

(defcustom skk-lookup-default-option-list
  '(exact exact prefix t ("$B!Z(B\\([^$B!Z![(B]+\\)$B![(B" . 1) "$B!&(B" nil)
  ;; CRCEN: $B;0>JF2(B $B%K%e!<%;%s%A%e%j!<1QOB!&?7%/%i%&%sOB1Q<-E5(B
  ;; KANJIGEN: Super$BE}9g<-=q(B99 Disk2/$B4A;z8;(B : EPWING
  ;; RIKAGAKU: $BM}2=3X<-E5(B
  ;; WAEI: what's this?
  "*$B%G%#%U%)%k%H$N<-=q8!:w!"J8;z@Z$j=P$7%*%W%7%g%s!#(B
$B$^$:<-=qL>$r%-!<$K$7$F(B `skk-lookup-option-alist' $B$r0z$-!"$=$3$K<-=q8!:w!"J8;z@Z(B
$B$j=P$7$N%*%W%7%g%s$,8+$D$+$l$P$=$l$r;HMQ$7!"8+$D$+$i$J$+$C$?>l9g$K$3$NJQ?t$G(B
$B;XDj$5$l$k<-=q8!:w!"J8;z@Z$j=P$7$N%*%W%7%g%s$r;HMQ$9$k!#(B

$B%j%9%H$N3FMWAG$O2<5-$NDL$j!#(B

  0th: $BAw$j$J$7JQ49$N:]$N(B search method $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(B
  1th: $BAw$j$"$jJQ49$G!"$+$D(B `skk-process-okuri-early' $B%*%W%7%g%s$r;XDj$7$F$$$J(B
       $B$$$H$-(B ($BAw$j2>L>7hDj$N8e$K8!:w$r3+;O$9$k$N$G!"Aw$j2>L>$,FCDj$G$-$k(B) $B$N(B
       search method $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(Bnil $B$r;XDj$9$k$H!"Aw$j(B
       $B$"$jJQ49$N:]$O$=$N<-=q$r8!:w$7$J$$!#(B
  2th: $BAw$j$"$jJQ49$G!"$+$D(B `skk-process-okuri-early' $B$G$"$k(B ($BAw$j2>L>7hDj$NA0(B
       $B$K8!:w$r3+;O$7$F$*$j!"Aw$j2>L>$,FCDj$G$-$J$$$N$G!"Aw$j2>L>$N$+$J(B prefix
       $B$r=|$$$?ItJ,$r8!:w%-!<$H$7$F(B lookup $B$KEO$7$F$$$k(B) $B$H$-$N(B search method
       $B$r<($9%7%s%\%k!#(Bregexp $B$O;XDjIT2D!#(Bnil $B$r;XDj$9$k$HAw$j$"$jJQ49$N:]$O$=(B
       $B$N<-=q$r8!:w$7$J$$!#(B
  3th: S $B<0!#$3$N(B S $B<0$rI>2A$7$F(B nil $B$K$J$k$H$-$O8!:w$7$J$$!#$"$k0lDj$N>r7o$rK~(B
       $B$7$?>l9g$K8!:w$7$J$$$h$&$K;XDj$G$-$k!#(B
  4th: `lookup-entry-heading' $B$,JV$9(B heading $B$+$i8uJd$H$7$F=PNO$9$kJ8;zNs$r@Z$j(B
       $B=P$9$?$a$N(B regexp $B;XDj5Z$S@Z$j=P$7%*%W%7%g%s!#(B
       car $B$K(B regexp $B$r<($9J8;zNs!"(Bcdr $B$K(B match-string $B$KEO$9(B count $B$r;XDj$9$k(B
       (4th $B$KJ8;zNs$@$1$r;XDj$7$?>l9g$O(B match-string $B$K$O(B 1 $B$,EO$5$l$k(B)$B!#(B
       cdr $BIt$K(B S $B<0$r;XDj$9$k$3$H$b2DG=!#2<5-$N$h$&$K(B cond $B<0$G>r7oH=Dj$9$l$P(B
       $BJ#?t$N(B regexp $B$r(B or $B;XDj$9$k$3$H$,2DG=!#(B

          (cond ((match-beginning 1) 1)
                ((match-beginning 2) 2)
	        ((match-beginning 3) 3)
                ((match-beginning 4) 4))

       cdr $BIt$NI>2A7k2L$,?t;z$N%j%9%H$K$J$k$H$-$O!"$=$N?t;z$r=g$K(B match-string
       $B$KEO$7$FJ8;zNs$r@Z$j=P$7!"$=$l$iO"7k$7$?J8;zNs$r8uJd$H$7$FJV$9!#Nc$($P!"(B

          (cond ((match-beginning 5) '(6 5)))

       $B$H;XDj$9$k$H!"(B(match-beginning 5) $B$,(B non-nil $B$K$J$C$?>l9g!"(B
       (match-string 6) $B$H(B (match-string 5) $B$r$=$N=g$KO"7k$7$?J8;zNs$r8uJd$H$7(B
       $B$F=PNO$9$k!#(B
       $B@Z$j=P$5$:$KJ8;zNsA4BN$rBP>]$K$9$k$H$-$O!"(B4th $B$K(B nil $B$r;XDj$9$k!#(B
  5th: $B@Z$j=P$5$l$?J8;zNs$NCf$K99$KJ#?t$N8uJd$r4^$`>l9g$N6h@Z$j$rI=$o$9(B
        regexp$B!#(B
       $BJ#?t$N8uJd$,F10l(B heading $B$NCf$K=PNO$5$l$J$$$H$-$O!"(Bnil $B$r;XDj$9$k!#(B
  6th: $B@Z$j=P$5$l$?J8;zNs$+$iFCDj$NJ8;zNs$r<h$j=|$/>l9g$K;XDj$9$k(B regexp$B!#(B
       $B<-=q$N=PNO$,<-=qFCM-$N5-9fJ8;z$r4^$`>l9g$K;XDj$9$k!#(B

$B$3$N%*%W%7%g%s$GBP1~$7$F$$$k<-=qL>$O!"(B\"CRCEN\", \"KANJIGEN\", \"RIKAGAKU\"
$B5Z$S(B \"WAEI\".
`lookup-entry-heading' $B$G<h$j=P$7$?J8;zNs$,2<5-$N$h$&$K$J$k$3$H$rA0Ds$K(B
$B$7$F$$$k!#(B

  \"$B$"!>$+!Z0!2J![!E%/%o(B\"
  \"$B$"$+!Zod2@![(B\"
  \"$B$3!>$7$g$&!Z>.@+!&>.@-![!E%7%d%&(B\"

`lookup-entry-heading' $B$,<+J,$N;HMQ$9$k<-=q$+$i$I$N$h$&$JJ8;zNs$r<h$j=P$9$N$+(B
$B3N$+$a$?$$$H$-$O!"(B`skk-lookup-pickup-headings' $B$r;HMQ$9$k!#Nc$($P!"(B

 (skk-lookup-pickup-headings \"$B$3$7$g$&(B\" 'exact)"
  :type '(list (choice :tag "Search method for okuri nasi"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const keyword) (const text)
		       (const nil))
	       (choice
		:tag "Search method for okuri ari (not process okuri early)"
		(const exact) (const prefix)
		(const suffix) (const substring)
		(const keyword) (const text)
		(const nil))
	       (choice :tag "Search method for okuri ari (process okuri early)"
		       (const exact) (const prefix)
		       (const suffix) (const substring)
		       (const keyword) (const text)
		       (const nil))
	       (sexp :tag "S expression to search")
	       (choice :tag "Regexp to substring candidate from heading"
		       (cons regexp sexp) (const nil))
	       (choice :tag "Regexp to split candidates"
		       regexp (const nil))
	       (choice :tag "Regexp to remove a string from candidates"
		       regexp (const nil)))
  :group 'skk-lookup)

(defcustom skk-lookup-search-modules nil
  "*$B8!:w%b%8%e!<%k$N@_Dj$N%j%9%H!#(B"
  :type '(repeat (cons :tag "Module" (string :tag "Name")
		       (repeat :tag "Dictionary" (string :tag "ID"))))
  :group 'skk-lookup)

(defcustom skk-lookup-process-henkan-key-function nil
  "*Lookup $B$KEO$9:]$K8!:w%-!<$r2C9)$9$k%U%!%s%/%7%g%s!#(B
$BAw$j$"$jJQ49$N:]$N$_%3!<%k$5$l$k!#0z?t$O2C9)$9$Y$-J8;zNs(B HENKAN-KEY$B!#(B
$BJV$jCM$O(B car $B$K2C9)$7$?J8;zNs!"(Bcdr $B$KAw$j2>L>$N2C9)J}K!$r<($9%^%8%C%/%J%s%P!<(B
$B$rF~$l$?(B cons cell$B!#(B
$B%^%8%C%/%J%s%P!<$O!"(B0 $B$,Aw$j$J$7$rI=$o$9(B ($BK\(B function $B$G$O;HMQ$9$k$3$H$O$J$$(B)$B!#(B
1 $B$OAw$j$"$jJQ49$G(B `skk-process-okuri-early' $B$,(B nil $B$N>l9g!#(B
2 $B$OAw$j$"$jJQ49$G(B `skk-process-okuri-early' $B$,(B non-nil $B$N>l9g$rI=$o$9!#(B
$B6a$$>-Mh!"(Bskk-lookup.el $BA4BN$rDL$8$F$3$N$h$&$J%^%8%C%/%J%s%P!<$r;H$o$J$$$h$&$K(B
$B2~NI$5$l$k2DG=@-$,$"$k!#(B"
  :type '(choice function (const nil))
  :group 'skk-hooks-and-functions)

(defcustom skk-lookup-kana-vector
  ["$B$!(B" "$B$"(B" "$B$#(B" "$B$$(B" "$B$%(B" "$B$&(B" "$B$'(B" "$B$((B" "$B$)(B" "$B$*(B"
   "$B$+(B" "$B$,(B" "$B$-(B" "$B$.(B" "$B$/(B" "$B$0(B" "$B$1(B" "$B$2(B" "$B$3(B" "$B$4(B"
   "$B$5(B" "$B$6(B" "$B$7(B" "$B$8(B" "$B$9(B" "$B$:(B" "$B$;(B" "$B$<(B" "$B$=(B" "$B$>(B"
   "$B$?(B" "$B$@(B" "$B$A(B" "$B$B(B" "$B$C(B" "$B$D(B" "$B$E(B" "$B$F(B" "$B$G(B" "$B$H(B" "$B$I(B"
   "$B$J(B" "$B$K(B" "$B$L(B" "$B$M(B" "$B$N(B"
   "$B$O(B" "$B$P(B" "$B$Q(B" "$B$R(B" "$B$S(B" "$B$T(B" "$B$U(B" "$B$V(B" "$B$W(B" "$B$X(B" "$B$Y(B" "$B$Z(B" "$B$[(B" "$B$\(B" "$B$](B"
   "$B$^(B" "$B$_(B" "$B$`(B" "$B$a(B" "$B$b(B"
   "$B$c(B" "$B$d(B" "$B$e(B" "$B$f(B" "$B$g(B" "$B$h(B"
   "$B$i(B" "$B$j(B" "$B$k(B" "$B$l(B" "$B$m(B"
   "$B$n(B" "$B$o(B" "$B$p(B" "$B$q(B" "$B$r(B" "$B$s(B"]
  "*skk-kana-rom-vector $B$N(B prefix $B$KBP1~$9$k$+$JJ8;z$N%Y%/%H%k!#(B
$B$"$k(B prefix $B$,$I$N$+$JJ8;z$KBP1~$9$k$+$N%^%C%W$r:n$k$?$a$K;2>H$9$k!#(B"
  :type 'sexp
  :group 'skk-lookup)

;;;; SKK-LOOKUP related internal variables.
(defvar skk-lookup-agent-list nil)
(defvar skk-lookup-default-module nil)
(defvar skk-lookup-module-list nil)
(defvar skk-lookup-prefix-and-kana-map nil)

;; SKK-ANNORTATION related variables.
(defcustom skk-show-annotation nil
  "*Non-nil $B$G$"$l$P!"Cp<a$rI=<($9$k!#(B
$B<-=q$N8uJd$K4^$^$l$k(B `;' $B0J9_$NJ8;zNs$rJQ49$N:]!"Cm5-$H$7$F(B\
$B%(%3!<%(%j%"!"$^$?$OJL(B Window $B$KI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-annotation
  :group 'skk-misc)

(defcustom skk-annotation-function nil
  "*annotation $B$rI=<($9$k$+$I$&$+$N%A%'%C%/;~$K%3!<%k$5$l$k4X?t!#(B
non-nil $B$rJV$9$H(B annotation $B$rI=<($9$k!#(Bannotation $B$NBP>]$H$9$kJ8;zNs(B
$B$r0z?t$K$7$F(B funcall $B$5$l$k!#(B"
  :type 'function
  :group 'skk-annotation
  :group 'skk-hooks-and-functions)

(defcustom skk-annotation-show-as-message t
  "*Non-nil $B$G$"$l$P!"Cm5-$r%(%3!<%(%j%"$KI=<($9$k!#(B"
  :type 'boolean
  :group 'skk-annotation
  :group 'skk-misc)

(defcustom skk-annotation-mode-hook nil
  "*SKK annotation mode $B$KF~$C$?$H$-$N%U%C%/!#(B"
  :type 'hook
  :group 'skk-annotation
  :group 'skk-hooks-and-functions)

;; SKK-ANNORTATION related internal constants and variables.
;; constants.
(defconst skk-annotation-buffer
  "*SKK annotation*")

;; global variables.
(defvar skk-annotation-mode-map nil
  "*SKK annotation $B%b!<%I$N%-!<%^%C%W!#(B")

(defvar skk-annotation-original-window-configuration nil
  "SKK annotation mode $B$KF~$kA0$N(B window configuration$B!#(B
skk-annotation-save-and-quit $B$r8F$V$H$3$N(B window configuration
$B$r;H$C$F(B SKK annotation mode $B$KF~$kA0$N(B window $B>uBV$KLa$9!#(B")

(defvar skk-annotation-annotated-word nil
  "annotation $B$rIU$1$i$l$kC18l!#(B")

;; buffer local variables.
(skk-deflocalvar skk-annotation-mode nil
  "Non-nil $B$G$"$l$P!"(Bannotation $B%b!<%I$G$"$k$3$H$r<($9!#(B")

;;; user variables.
(defface skk-dcomp-face
  '((((class color)) (:foreground "DarkKhaki"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :italic t)))
  "*Face used to highlight region dynamically completed."
  :group 'skk-dcomp
  :group 'skk-faces)

(defcustom skk-dcomp-activate t
  "*Non-nil $B$G$"$l$P8+=P$78l$N%@%$%J%_%C%/%3%s%W%j!<%7%g%s$N5!G=$rM-8z$K$9$k!#(B"
  :type 'boolean
  :group 'skk-dcomp)

(defcustom skk-dcomp-face-priority 700
  "*Overlay/extent priority of `skk-dcomp-face'."
  :type 'integer
  :group 'skk-dcomp)

(defcustom skk-dcomp-keep-completion-keys nil
  ;;   (delq
  ;;    nil
  ;;    (list
  ;;     (car (rassoc (list nil 'skk-toggle-kana)
  ;;                  skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters)
  ;;                  skk-rom-kana-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-kana)
  ;;                  skk-rom-kana-base-rule-list))
  ;;     (car (rassoc (list nil 'skk-toggle-characters)
  ;;                  skk-rom-kana-base-rule-list))))
  "*$B<+F0%3%s%W%j!<%7%g%s$5$l$?8+=P$78l$r>C$5$J$$%-!<$N%j%9%H!#(B
$BDL>o$O8+=P$78l$N%3%s%W%j!<%7%g%s8e!"<!$N%-!<F~NO$r$9$k$H!"<+F0%3%s%W(B
$B%j!<%7%g%s$5$l$?%-!<F~NO$,>C$($F$7$^$&$,!"$3$N%j%9%H$K;XDj$5$l$?%-!<(B
$BF~NO$,$"$C$?$H$-$O<+F0%3%s%W%j!<%7%g%s$5$l$?8+=P$78l$r>C$5$J$$!#(B"
  :type '(choice (repeat string) (const nil))
  :group 'skk-dcomp
  :group 'skk-filenames)

;;; internal variables and constants.
(skk-deflocalvar skk-dcomp-start-point nil)
(skk-deflocalvar skk-dcomp-end-point nil)
(skk-deflocalvar skk-dcomp-extent nil)
(defvar skk-dcomp-face 'skk-dcomp-face)

;; SKK-DIC related internal constants and variables.

(defvar skk-dic-comp-first nil)

(require 'product)
(product-provide
    (provide 'skk-vars)
  (require 'skk-version))

;;; skk-vars.el ends here
