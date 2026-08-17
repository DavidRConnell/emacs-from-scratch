;;; org-tweaks.el --- Alter default org behavior -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Largely lifted from doom's setup as I think that is much better than
;; the default ways.

;;; Code:

(defun my-org-insert-item (direction)
  (cond ((org-at-item-p)
	 (org-beginning-of-item)
	 (org-insert-item (org-element-property :checkbox
						(org-element-at-point))))
	((org-at-table-p) (org-table-insert-row))
	(t
	 (let ((level (or (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
	   (let ((todo-keyword (org-element-property :todo-keyword
						     (org-element-at-point))))
             (org-back-to-heading)
             (insert (make-string level ?*) " ")
             (save-excursion (insert "\n"))
	     (when (string= todo-keyword "DONE")
	       (setq todo-keyword "TODO"))
	     (when (stringp todo-keyword) (insert todo-keyword " "))))))

  (when (eq direction 'below)
    (message "metadown")
    (org-metadown)))

(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (my-org-insert-item 'below)))

(defun +org/insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (my-org-insert-item 'above)))

(provide 'org-tweaks)
;;; org-tweaks.el ends here
