;;; install-info.el -- install-info in Emacs Lisp
;; Copyright (C) 2000 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: docs, help, info
;; Last Modified: Oct 31 2000

;; This file is not part of GNU Emacs.

;; This file is free software;  you can redistribute it and/or modify it  under
;; the  terms  of  the  GNU General Public License  as  published  by  the Free
;; Software  Foundation;  either versions  2,  or  (at your option)  any  later
;; version.

;; This file is distributed in the hope that it will be useful  but WITHOUT ANY
;; WARRANTY;  without even the implied warranty  of MERCHANTABILITY  or FITNESS
;; FOR  A  PARTICULAR PURPOSE.  See  the  GNU General Public License  for  more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs,  see  the  file  COPYING.  If not,  write  to  the  Free Software
;; Foundation Inc., 59 Temple Place - Suite 330, Boston,  MA 02111-1307, USA.

;;; Commentary:

;; This program updates info/dir entries, like install-info in GNU texinfo.

;;; Code:

(eval-when-compile
  (require 'cl))

(condition-case nil
    (require 'jka-compr)
  (error))

(defun install-info (info-file dir-file &optional entry section delete)
  "Install or delete dir entries from INFO-FILE in the Info directory file
DIR-FILE.

Optioinal third arg ENTRY specifies the text inserted as an Info directory
entry. ENTRY should have the form of an Info menu item line plus zero or more
extra lines starting with whitespace. If you specify more than one entry (i.e.
list of entries), they are all added. If you don't specify any entries, they
are determined from information in the Info file itself.

If optional fourth arg SECTION is given, put this file's entries in section
SECTION of the directory. If you specify more than one section (i.e. list of
sections), all the entries are added in each of the sections. If you don't
specify any sections, they are determined from information in the Info file
itself.

If optional fifth arg DELETE is non-nil, delete existing entries for INFO-FILE
from DIR-FILE; don't insert any new entries."
  (let ((buf (generate-new-buffer " *tmp*"))
	entries)
    ;;
    (if (stringp info-file)
	(setq info-file (expand-file-name info-file))
      (error "%s" "No input file specified."))
    (unless (file-exists-p info-file)
      (error "No such file or directory for %s" info-file))
    ;;
    (if (stringp dir-file)
	(setq dir-file (expand-file-name dir-file))
      (error "%s" "No dir file specified."))
    ;;
    (when (stringp entry)
      (setq entry (list entry)))
    (when (stringp section)
      (setq section (list section)))
    ;;
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (install-info-insert-file-contents info-file))
    ;;
    (cond
     ((and entry section)
      (setq entries (install-info-entries section entry)))
     (entry
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "^INFO-DIR-SECTION " nil t)
	  (end-of-line)
	  (setq section
		(cons (buffer-substring (match-end 0) (point)) section))))
      (unless section
	(setq section (list "Miscellaneous")))
      (setq entries (install-info-entries section entry)))
     (section
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "^START-INFO-DIR-ENTRY" nil t)
	  (forward-line 1)
	  (beginning-of-line)
	  (while (not (looking-at "^END-INFO-DIR-ENTRY"))
	    (let (start end str)
	      (setq start (point))
	      (unless (eolp)
		(end-of-line)
		(setq str (buffer-substring start (point)))
		(if (string-match "^* " str)
		    (setq entry (cons str entry))
		  (when entry
		    (setq entry
			  (cons (format "%s\n%s" (car entry) str)
				(cdr entry))))))
	      (forward-line 1)
	      (beginning-of-line)))))
      (unless (setq entry (nreverse entry))
	(error "warning; no info dir entry in %s" info-file))
      (setq entries (install-info-entries section entry)))
     (t
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "^INFO-DIR-SECTION " nil t)
	  (let (sec entry)
	    (end-of-line)
	    (setq sec (buffer-substring (match-end 0) (point)))
	    (forward-line 1)
	    (beginning-of-line)
	    (when (looking-at "^START-INFO-DIR-ENTRY")
	      (forward-line 1)
	      (beginning-of-line)
	      (while (not (looking-at "^END-INFO-DIR-ENTRY"))
		(let (start end str)
		  (setq start (point))
		  (unless (eolp)
		    (end-of-line)
		    (setq str (buffer-substring start (point)))
		    (if (string-match "^* " str)
			(setq entry (cons str entry))
		      (when entry
			(setq entry
			      (cons (format "%s\n%s" (car entry) str)
				    (cdr entry))))))
		  (forward-line 1)
		  (beginning-of-line))))
	    (when (and sec (setq entry (nreverse entry)))
	      (setq entries
		    (nconc entries
			   (install-info-entries (list sec) entry))))))
	;;
	(unless entries
	  (goto-char (point-min))
	  (while (re-search-forward "^START-INFO-DIR-ENTRY" nil t)
	    (forward-line 1)
	    (beginning-of-line)
	    (while (not (looking-at "^END-INFO-DIR-ENTRY"))
	      (let (start end str)
		(setq start (point))
		(unless (eolp)
		  (end-of-line)
		  (setq str (buffer-substring start (point)))
		  (if (string-match "^* " str)
		      (setq entry (cons str entry))
		    (when entry
		      (setq entry
			     (cons (format "%s\n%s" (car entry) str)
				   (cdr entry))))))
		(forward-line 1)
		(beginning-of-line))))
	  (unless (setq entry (nreverse entry))
	    (error "warning; no info dir entry in %s" info-file))
	  (unless section
	    (setq section (list "Miscellaneous")))
	  (setq entries (install-info-entries section entry))))))
    ;;
    (if delete
	(install-info-delete-entries entries dir-file)
      (install-info-add-entries (nreverse entries) dir-file))
    ;;
    (kill-buffer buf)))

(defun install-info-entries (section entry)
  (let (entries)
    (dolist (sec section)
      (dolist (en entry)
	(setq entries
	      (nconc entries (list (cons sec en))))))
    entries))

(defun install-info-delete-entries (entries dir)
  (setq dir (expand-file-name dir))
  (let ((buf (generate-new-buffer " *dir*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (if (not (file-exists-p dir))
	  (error "%s is non-existent" dir)
	(install-info-insert-file-contents dir)
	(dolist (entry entries)
	  (let ((en (cdr entry))
		key)
	    (when (string-match ")" en)
	      (setq key (substring en 0 (match-beginning 0))))
	    (goto-char (point-min))
	    (while (re-search-forward
		    (concat "^" (regexp-quote key) "\\(\\.info\\)?)") nil t)
	      (let ((start (match-beginning 0)))
		(when (eq 0 (forward-line 1))
		  (beginning-of-line))
		(while (not (or (eolp)
				(looking-at "^* ")))
		  (when (eq 0 (forward-line 1))
		    (beginning-of-line)))
		(delete-region start (point)))))))
      (install-info-write-region (point-min) (point-max) dir))
    (kill-buffer buf)))

(defun install-info-add-entries (entries dir)
  (setq dir (expand-file-name dir))
  (let ((buf (generate-new-buffer " *dir*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (if (file-exists-p dir)
	  (install-info-insert-file-contents dir)
	(insert "This is the file .../info/dir, which contains the
topmost node of the Info hierarchy, called (dir)Top.
The first time you invoke Info you start off looking at this node.
\037
File: dir,	Node: Top	This is the top of the INFO tree

  This (the Directory node) gives a menu of major topics.
  Typing \"q\" exits, \"?\" lists all Info commands, \"d\" returns here,
  \"h\" gives a primer for first-timers,
  \"mEmacs<Return>\" visits the Emacs manual, etc.

  In Emacs, you can click mouse button 2 on a menu item or cross reference
  to select it.

* Menu:

"))
      (dolist (entry entries)
	(let ((sec (car entry))
	      (en (cdr entry))
	       key)
	  (when (string-match ")" en)
	    (setq key (substring en 0 (match-beginning 0))))
	  (goto-char (point-min))
	  (cond
	   ((re-search-forward (concat "^" sec "$") nil t)
	    (let (pt)
	      (if (eq 1 (forward-line 1))
		  (newline 1)
		(beginning-of-line))
	      (setq pt (point))
	      (while (not (eolp))
		(cond
		 ((looking-at
		   (concat "^" (regexp-quote key) "\\(\\.info\\)?)"))
		  (let ((start (point)))
		    (when (eq 0 (forward-line 1))
		      (beginning-of-line))
		    (while (not (or (eolp)
				    (looking-at "^* ")))
		      (when (eq 0 (forward-line 1))
			(beginning-of-line)))
		    (delete-region start (point))))
		 (t
		  (when (eq 0 (forward-line 1))
		    (beginning-of-line)))))
	      (goto-char pt)
	      (insert (format "%s\n" en))))
	   (t
	    (goto-char (point-max))
	    (unless (bolp)
	      (newline 1))
	    (insert (format "\n%s\n%s\n" sec en))))))
      (install-info-write-region (point-min) (point-max) dir))
    (kill-buffer buf)))

(defun install-info-compressed-p (file)
  (if (not (featurep 'jka-compr))
      nil
    (let ((list jka-compr-compression-info-list)
	  tag)
      (while (and list (not tag))
	(when (string-match (aref (car list) 0) file)
	  (setq tag t))
	(setq list (cdr list)))
      tag)))

(defun install-info-insert-file-contents (file &optional visit beg end replace)
  (let ((coding-system-for-read 'raw-text))
    (if (install-info-compressed-p file)
	(jka-compr-insert-file-contents file visit beg end replace)
      (insert-file-contents file visit beg end replace))))

(defun install-info-write-region (start end file &optional append visit)
  (let ((coding-system-for-write 'raw-text))
    (if (install-info-compressed-p file)
	(jka-compr-write-region start end file append visit)
      (write-region start end file append visit))))

;;

(provide 'install-info)

;;; install-info.el ends here
