;;; dot.emacs --- SKK related customization in ~/.emacs  -*- emacs-lisp -*-

;;; Commentary:

;; ~/.emacs $B$KDI2C$9$k$?$a$N@_DjNc$G$9!#(B

;;; Code:

;; @@ $B4pK\$N@_Dj(B

;; Mule 2.3 (Emacs 19) $B$r;H$C$F$$$k>l9g$OI,MW(B
;; (require 'skk-setup)

;; $B%+%?%+%J(B/$B$R$i$,$J(B $B%-!<$G(B SKK $B$r5/F0$9$k(B
(global-set-key [hiragana-katakana] 'skk-mode)

;; ~/.skk $B$K$$$C$Q$$@_Dj$r=q$$$F$$$k$N$G%P%$%H%3%s%Q%$%k$7$?$$(B
(setq skk-byte-compile-init-file t)
;; $BCm(B) $B0[$J$k<oN`$N(B Emacsen $B$r;H$C$F$$$k>l9g$O(B nil $B$K$7$^$9(B

;; SKK $B$r(B Emacs $B$N(B input method $B$H$7$F;HMQ$9$k(B
(setq default-input-method "japanese-skk")

;; SKK $B$r5/F0$7$F$$$J$/$F$b!"$$$D$G$b(B skk-isearch $B$r;H$&(B
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

;; @@ $B1~MQE*$J@_Dj(B

;; ~/.skk* $B$J%U%!%$%k$,$?$/$5$s$"$k$N$G@0M}$7$?$$(B
(if (not (file-directory-p "~/.ddskk"))
    (make-directory "~/.ddskk"))
(setq skk-init-file "~/.ddskk/init.el"
      skk-custom-file "~/.ddskk/custom.el"
      skk-emacs-id-file "~/.ddskk/emacs-id"
      skk-record-file "~/.ddskk/record"
      skk-jisyo "~/.ddskk/jisyo"
      skk-backup-jisyo "~/.ddskk/jisyo.bak")
;; $BCm(B) SKK $B$N8D?M<-=q$O(B skkinput $B$J$I$N%W%m%0%i%`$G$b;2>H$7$^$9$+$i!"(B
;;     $B>e5-$N@_Dj$r$7$?>l9g$O$=$l$i$N%W%m%0%i%`$N@_Dj%U%!%$%k$b=q$-(B
;;     $B$+$($kI,MW$,$"$j$^$9!#(B

;; migemo $B$r;H$&$+$i(B skk-isearch $B$K$O$*$H$J$7$/$7$F$$$FM_$7$$(B
(setq skk-isearch-start-mode 'latin)

;; super-smart-find $B$N$?$a$N@_Dj(B ($B0UL#$"$k$+$J!)(B)
(setq super-smart-find-self-insert-command-list
      '(canna-self-insert-command
	egg-self-insert-command
	self-insert-command
	tcode-self-insert-command-maybe
	skk-insert))

;; YaTeX $B$N$H$-$@$16gFIE@$rJQ99$7$?$$(B
(add-hook 'yatex-mode-hook
	  (lambda ()
	    (require 'skk)
	    (setq skk-kutouten-type 'en)))

;; $B$+$J%b!<%I$NF~NO$G(B ($B%b!<%IJQ99$r9T$J$o$:$K(B) $BD92;(B($B!<(B)$B$r(B
;; ASCII $B?t;z$ND>8e$G$O(B `-' $B$K!"A43Q?t;z$ND>8e$G$O(B `$B!](B' $B$K$7$?$$!#(B
(setq skk-rom-kana-rule-list
	  (cons '("-" nil skk-hyphen)
			skk-rom-kana-rule-list))
(defun skk-hyphen (arg)
  (interactive "P")
  (let ((c (char-before (point))))
    (cond ((and (<= ?0 c) (>= ?9 c)) "-")
	  ((and (<= ?$B#0(B c) (>= ?$B#9(B c)) "$B!](B")
	  (t "$B!<(B"))))

;;; dot.emacs ends here
