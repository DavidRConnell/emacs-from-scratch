;;; my-cypher-mode.el --- cypher querying -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: December 14, 2025

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
;; Set up writing cypher queries.

;;; Code:

(autoload 'cypher-mode "cypher-mode")
(add-to-list 'auto-mode-alist "\\.cypehr\\'" . cypher-mode)

(with-eval-after-load 'cypher-mode
  (set-face-attribute 'cypher-variable-face nil
		      :foreground 'unspecified :background 'unspecified
		      :family 'unspecified :slant 'unspecified
		      :weight 'normal :height 'unspecified
		      :underline 'unspecified :overline 'unspecified
		      :box 'unspecified :inherit font-lock-variable-name-face)
  (set-face-attribute 'cypher-pattern-face nil
		      :foreground 'unspecified :background 'unspecified
		      :family 'unspecified :slant 'unspecified
		      :weight 'bold :height 'unspecified
		      :underline 'unspecified :overline 'unspecified
		      :box 'unspecified :inherit font-lock-variable-name-face)

  (defun my-cypher-send-buffer (&optional output-file)
    (interactive)
    (let ((file (buffer-name (current-buffer)))
	  (database "neo4j")
	  (command "cypher-shell --database=%s --file=%s"))
      (setq command (format command database file))
      (if output-file
	  (setq command (concat command " > " output-file)))
      (async-shell-command command)))

  (general-def
    :keymaps 'cypher-mode-map
    :prefix "C-c"
    "C-c" 'my-cypher-send-buffer
    "C-e" (defun my-cypher-results-to-buffer (file)
	    (interactive "F")
	    (my-cypher-send-buffer file))))


(provide 'my-cypher-mode)
;;; my-cypher-mode.el ends here
