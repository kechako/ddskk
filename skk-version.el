;;; skk-version.el -- Version information for SKK.
;;
;; Copyright (C) 2000, 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;;
;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-version.el,v 1.7.2.2 2001/09/11 12:15:16 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/09/11 12:15:16 $
;;
;; This file is part of Daredevil SKK.
;;
;; Daredevil SKK is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.
;;
;; Daredevil SKK is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;; This is a copy of elmo-version.el and wl-version.el...

;;; Code:
;;
(require 'product)
(provide 'skk-version) ; have to declare in the top.

(product-provide 'skk-version
  (product-define "Daredevil SKK Ne" nil '(11 5 0 1) "Hotarugaike"))

;; set version-string
(if (fboundp 'product-version-as-string)
    (product-version-as-string 'skk-version)
  (product-string-1 'skk-version))

;;;###autoload
(defun skk-version (&optional without-codename)
  "Return SKK version with its codename.
If WITHOUT-CODENAME is non-nil, simply return SKK version without the codename."
  (interactive "P")
  (if (interactive-p)
      (message "%s" (skk-version without-codename))
    (product-string-1 'skk-version (not without-codename))))

;; for backward compatibility
;;(defconst skk-version (product-version-string (product-find 'skk-version)))
;;(make-obsolete-variable
;; 'skk-version
;; "use (product-version-string (product-find 'skk-version)) instead.")
;;
;; (defconst skk-codename (product-code-name (product-find 'skk-version)))
;; (make-obsolete-variable
;;  'skk-codename
;;  "use (product-code-name (product-find 'skk-version)) instead.")
;;
;; (defconst skk-major-version (string-to-int (substring skk-version 0 2)))
;; (defconst skk-minor-version (string-to-int (substring skk-version 3)))
;; (defconst skk-branch-name "Daredevil")

;;; skk-version.el ends here
