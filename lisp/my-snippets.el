;;; my-snippets.el --- Snippet set up -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: December 12, 2025

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
;; Load snippets using YAS.

;;; Code:

(require 'my-variables)
(require 'my-keybindings)

(customize-set-variable 'yas-snippet-dirs
			(directory-files my-snippets-dir 'full
					 (rx bol (in "0-9" "a-z" "A-Z"))))

(dolist (fn '(yas-expand
	      yas-lookup-snippet
	      yas-insert-snippet
	      yas-new-snippet
	      yas-visit-snippet-file))
  (autoload fn "yasnippet" nil t))

(general-imap "C-e" 'yas-expand)

(general-def
  :keymaps 'my-yas-map
  "v" 'yas-visit-snippet-file
  "n" 'yas-new-snippet
  "r" 'yas-reload-all)

(with-eval-after-load 'yasnippet
  (let ((load-path (append (list (expand-file-name "doom" my-snippets-dir))
			   load-path)))
    (require 'doom-snippets))

  (general-def
    :keymaps 'yas-keymap
    "M-n" 'yas-next-field-or-maybe-expand
    "M-p" 'yas-prev-field)

  (with-eval-after-load 'cape
    (require 'yasnippet-capf)
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))

  (yas-global-mode))

(require 'yatemplate)
(with-eval-after-load 'yatemplate
  (require 'yasnippet)

  (customize-set-variable 'yatemplate-dir
			  (expand-file-name "file-templates"
					    user-emacs-directory))
  (auto-insert-mode)
  (yatemplate-fill-alist))

(provide 'my-snippets)
;;; my-snippets.el ends here
