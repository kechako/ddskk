;;; skk-server-completion.el --- server completion �Υ��饤�����
;;
;; Copyright (C) 2005 Fumihiko MACHIDA <machida@users.sourceforge.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA

;;; Commentary:

;; Server completion ���б��������񥵡��Ф��Ѥ����Ф��줫��Ϥޤ����Ƥ�
;; ���θ�����Ԥʤ��ޤ���

;; ���Υץ����ϰʲ��� 2 �Ĥε�ǽ���󶡤��ޤ���
;;
;; * skk-look �����ܸ��ǡ��ɤߤκǸ�� `~' ���դ����Ѵ�����ȡ������ɤߤ�
;;   ��Ϥޤ����Ƥθ����ɽ�����ޤ���
;;
;; �㡧
;;
;; ���ޤ���~
;; ==> "�ޤ���" "Į��" "�ޤ�������" "Į�ı�" "�ޤ����������夦" "Į�ľ��ĵ�" ..
;;
;; * skk-comp �ǡ�server completion �����
;;
;; �㡧
;;
;; ���ޤ���-!- �� Tab �򲡤��ȡ����ޤ������� �� ���ޤ����������夦 �ġ�
;; �Ȥʤ�ޤ���

;; [������ˡ]
;;
;; .skk �ˡ��ʲ����ɲä��ޤ���
;;
;; (require 'skk-server-completion)
;; (add-to-list 'skk-search-prog-list
;;	     '(skk-server-completion-search) t)
;;
;; �ޤ���`~' ���դ����Ѵ���̤�Ŀͼ���˳ؽ����Ƥ��ޤ��Τ���뤿��ˤ�
;; �ʲ����ɲä��Ƥ���������
;;
;; (add-hook 'skk-search-excluding-word-pattern-function
;;	  #'(lambda (kakutei-word)
;;	      (string-match (format "%s$"
;;				    (regexp-quote
;;				     (char-to-string
;;				      skk-server-completion-search-char)))
;;			    skk-henkan-key)))

;;; Code:

(require 'skk)
(require 'skk-comp)


;;;###autoload
(defun skk-server-completion-search ()
  "�����С�����ץ꡼������Ԥ�������줿�Ƹ��Ф��Ǥ���˸������롣
����ͭ���Ѵ��ˤ����б���"
  (when (and (eq (aref skk-henkan-key (1- (length skk-henkan-key)))
		 skk-server-completion-search-char)
	     (not (or skk-henkan-okurigana
		      skk-okuri-char)))
    (let ((key (substring skk-henkan-key
			  0 (1- (length skk-henkan-key))))
	  midasi-list)
      (when skk-use-numeric-conversion
	(setq key (skk-num-compute-henkan-key key)))
      (setq midasi-list (skk-server-completion-search-midasi key))
      (skk-server-completion-search-recursive midasi-list))))

(defun skk-server-completion-search-midasi (key)
  "server completion �����Ѥ��ơ�key ����Ϥޤ뤹�٤Ƥθ��Ф���Υꥹ�Ȥ��ֵѤ��롣"
  (when (skk-server-live-p (skk-open-server))
    (with-current-buffer skkserv-working-buffer
      (let ((cont t)
	    (count 0)
	    l)
	(erase-buffer)
	(process-send-string skkserv-process (concat "4" key " "))
	(while (and cont (skk-server-live-p))
	  (accept-process-output)
	  (setq count (1+ count))
	  (when (> (buffer-size) 0)
	    (if (eq (char-after 1) ?1)	;?1
		;; found key successfully, so check if a whole line
		;; is received.
		(when (eq (char-after (1- (point-max)))
			  ?\n)		;?\n
		  (setq cont nil))
	      ;; not found or error, so exit
	      (setq cont nil))))
	(goto-char (point-min))
	(when skk-server-report-response
	  (skk-message "%d �� SKK �����С��α����Ԥ��򤷤ޤ���"
		       "Waited for server response %d times"
		       count))
	(when (eq (following-char) ?1)	;?1
	  (forward-char 2)
	  (car (skk-compute-henkan-lists nil)))))))

(defun skk-server-completion-search-recursive (midasi-list)
  "`midasi-list' �θ��Ф�����Ѵ�����"
  (let (result-list kouho-list)
    (dolist (skk-henkan-key midasi-list)
      (setq kouho-list (cons skk-henkan-key (skk-search-server-1 nil nil))
	    result-list (nconc result-list kouho-list)))
    result-list))

;;;###autoload
(defun skk-comp-by-server-completion ()
  ;; skk-comp-prefix �ϻȤ��ʤ�
  ;; ���ΤȤ���������Ѵ��Τ�
  (when skk-comp-first
    (setq skk-server-completion-words
	  (skk-server-completion-search-midasi skk-comp-key))
    (when (string= skk-comp-key
		   (car skk-server-completion-words))
      (pop skk-server-completion-words)))
  (pop skk-server-completion-words))

(provide 'skk-server-completion)


;;; skk-server-completion.el ends here
