;;; dot.emacs --- SKK related customization in ~/.emacs  -*- emacs-lisp -*-

;; @@ $B4pK\$N@_Dj(B

(require 'skk-autoloads)

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

;;; dot.emacs ends here
