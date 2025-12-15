;;; my-text-mode.el --- General text-mode set up -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: December 13, 2025

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
;; Configuration common to all text modes. Some smaller `text-mode' type modes
;; may also be configured here to reduce having an excessive number of smaller
;; files.
;;
;; Also contains some set up that isn't strictly for `text-mode's but is in the
;; spirit of writing (like spellchecking that works in `prog-mode' comments as
;; well as `text-mode's).

;;; Code:

(require 'my-keybindings)
(require 'my-straight-bootstrap)
(require 'my-ui)
(require 'my-completion)

(require 'flymake-proselint)
(add-hook 'text-mode-hook #'flymake-proselint-setup)
(add-hook 'text-mode-hook #'visual-line-mode)

(customize-set-variable 'flymake-proselint-disable
			;; Tends to catch preamble/header arguments.
			'(typography.exclamation))

(defun my-maybe-turn-on-flymake ()
  "Selectively turn on flymake in `text-mode'.

Prevents `flymake-proselint' from showing errors in note files."
  (let* ((current-file (file-name-nondirectory (buffer-file-name)))
	 (in-roam-note-p (or (string= current-file "todo.org")
			     (and (featurep 'org-roam)
				  (org-roam-file-p current-file)))))
    (unless in-roam-note-p
      (flymake-mode))))

(add-hook 'text-mode-hook #'my-maybe-turn-on-flymake)

(add-hook 'text-mode-hook
	  (defun my-text-mode-capfs ()
	    (setq-local completion-at-point-functions
			(list (cape-capf-sort
			       (cape-capf-super #'cape-dabbrev
						#'ispell-completion-at-point))))))

(straight-use-package
 '(sdcv-mode :type git :host github :repo "gucong/emacs-sdcv"))

(autoload 'sdcv-search "sdcv-mode" nil t)
(autoload 'wiki-summary "wiki-summary" nil t)
(autoload 'wordnut-search "wordnut" nil t)

(my-popper-add-reference "\\*wiki-summary\\*.*")
(my-popper-add-reference "\\*sdcv\\*")

(general-def
  :keymaps 'my-dictionary-map
  "d" 'sdcv-search
  "k" 'wiki-summary
  "w" 'wordnut-search)

(with-eval-after-load 'sdcv-mode
  (general-nmap
    :keymaps 'sdcv-mode-map
    "q" 'evil-delete-buffer
    "C-SPC" 'sdcv-toggle-entry))

(with-eval-after-load 'wordnut
  (general-nmap
    :keymaps 'wordnut-mode-map
    "q" 'evil-delete-buffer))

(setenv "ASPELL_CONF"
	(format "dict-dir %s"
		(expand-file-name
		 "lib/aspell"
		 (file-name-parent-directory
		  (file-name-parent-directory (executable-find "aspell"))))))

(require 'jinx)
(global-jinx-mode)

(general-nmap
  "z=" 'jinx-correct)

(set-face-attribute 'jinx-misspelled nil
		    :underline t
		    :inherit 'nano-face-popout)

(autoload 'nov-mode "nov")
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(with-eval-after-load 'nov
  (customize-set-variable 'nov-text-width fill-column))

(provide 'my-text-mode)
;;; my-text-mode.el ends here
