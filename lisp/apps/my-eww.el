;;; my-eww.el --- Refuge for random apps -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: December 09, 2025

;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; Add eww readable to improve eww display of web pages.

;;; Code:

(require 'eww)

(defun mozilla-readable (url)
  (let ((buff (format "*readable-%s*" url)))
    (with-current-buffer (get-buffer-create buff)
      (pop-to-buffer (current-buffer))
      (shell-command (format "readable %s" url) (current-buffer))
      (eww-display-html 'utf-8 (buffer-name) nil (point-min) (current-buffer)))))

(defun eww-readable ()
  "View the main \"readable\" parts of the current web page.
This command uses heuristics to find the parts of the web page that
contains the main textual portion, leaving out navigation menus and
the like."
  (interactive nil eww-mode)
  (let* ((old-data eww-data)
	 (dom (with-temp-buffer
		(insert (plist-get old-data :source))
		(condition-case nil
		    (decode-coding-region (point-min) (point-max) 'utf-8)
		  (coding-system-error nil))
		(shell-command-on-region (point-min) (point-max)
					 "readable" nil t)
                (eww--preprocess-html (point-min) (point-max))
		(libxml-parse-html-region (point-min) (point-max))))
         (base (plist-get eww-data :url)))
    (eww-score-readability dom)
    (eww-save-history)
    (eww-display-html nil nil
		      (list 'base (list (cons 'href base))
                            (eww-highest-readability dom))
		      nil (current-buffer))
    (dolist (elem '(:source :url :title :next :previous :up :peer))
      (plist-put eww-data elem (plist-get old-data elem)))
    (eww--after-page-change)))

(provide 'my-eww)
;;; my-eww.el ends here
