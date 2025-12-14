;;; my-sh-mode.el --- Shell editing -*- lexical-binding: t; -*-

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
;; Configuration for editing shell scripts.

;;; Code:

(require 'my-keybindings)
(require 'my-prog-mode "modes/my-prog-mode")

(autoload 'bash-ts-mode "bash-ts-mode")
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env bash\\'" . sh-mode))
(add-hook 'sh-base-mode-hook #'eglot-ensure)

(add-hook 'sh-base-mode-hook
	  (defun my-set-shell-formatter ()
	    (setq-local format-all-formatters '(("Shell" shfmt)))))
(add-hook 'sh-base-mode-hook #'format-all-mode)

(with-eval-after-load 'bash-ts-mode
 (customize-set-variable 'sh-indent-after-continuation 'always)
 (general-nmmap
   :keymaps 'sh-base-mode-map
   "gK" (lambda (arg)
	   (interactive "P")
	   (my-man-at-point arg 1))))

(dolist (fn '(bats-run-current-test
	      bats-run-current-file
	      bats-run-all))
  (autoload fn "bats-mode" nil t))
(autoload 'bats-mode "bats-mode")
(add-to-list 'auto-mode-alist '("\\.bats\\'" . bats-mode))

(defvar my-bats-map (make-sparse-keymap))

(general-def
  :keymaps 'my-bats-map
  "d" 'bats-run-current-test
  "f" 'bats-run-current-file
  "t" 'bats-run-all)

(with-eval-after-load 'bash-ts-mode
  (my-local-leader-def
    :keymaps 'sh-base-mode-map
    "t" '(:keymap my-bats-map :which-key "test")))

(with-eval-after-load 'bats-mode
  (general-nmap
   :keymaps 'bats-mode-map
   :prefix "C-c"
   "C-c" 'bats-run-current-test
   "C-b" 'bats-run-current-file)

  (my-local-leader-def
   :keymaps 'bats-mode-map
   "t" '(:keymap my-bats-map :which-key "test")))

(provide 'my-sh-mode)
;;; my-sh-mode.el ends here
