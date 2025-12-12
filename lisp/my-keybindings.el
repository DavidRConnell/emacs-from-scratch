;;; my-keybindings.el --- Evil key bindings -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: October 26, 2020

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
;; Evil and general keybindings.

;;; Code:

;; Must be set before loading evil. Needed to work with evil-collections.
(customize-set-variable 'evil-want-keybinding nil)

;; Manually set xref later; don't need evil's.
(customize-set-variable 'evil-collection-want-find-usages-bindings nil)

(require 'general)
(require 'evil)
(require 'evil-collection)
(require 'which-key)

(let ((load-path (append (list (expand-file-name "vendor/nano-emacs"
						 user-emacs-directory))
			 load-path)))
  (require 'nano-base-colors))

(general-evil-setup)

(general-create-definer my-leader-def
  :states '(normal visual motion)
  :keymaps 'override
  :prefix "SPC")

(general-create-definer my-local-leader-def
  :states '(normal visual motion)
  :keymaps 'override
  :prefix "C-SPC")

(general-create-definer general-nmmap :states '(normal motion))
(general-create-definer general-nmvmap :states '(normal motion visual))

(dolist (command my-extra-evil-jump-commands)
  (evil-set-command-property command :jump t))

(customize-set-variable 'evil-echo-state nil)
(customize-set-variable 'evil-mode-line-format nil)
(customize-set-variable 'evil-symbol-word-search t)
(customize-set-variable 'evil-ex-search-vim-style-regexp t)
(customize-set-variable 'evil-undo-system 'undo-redo)

(setq evil-echo-area-message nil
      evil-visual-state-cursor nano-color-salient
      evil-emacs-state-cursor nano-color-critical
      evil-normal-state-cursor nano-color-foreground)

(evil-select-search-module 'evil-search-module 'evil-search)

(general-nmvmap
  :keymaps 'override
  "J" 'evil-scroll-line-down
  "K" 'evil-scroll-line-up
  "H" 'evil-beginning-of-visual-line
  "L" 'evil-end-of-line-or-visual-line
  "M" 'evil-goto-mark)

(general-imap
  "C-u" 'evil-delete-back-to-indentation
  "C-SPC" #'completion-at-point)

(general-nmvmap
  "gj" 'evil-join
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "-" 'dired-jump
  "C-m" 'evil-goto-mark
  "C-q" 'evil-execute-macro)

(my-leader-def
  "b" 'switch-to-buffer
  "w" 'save-buffer
  "q" (defun my-kill-buffer () (interactive) (kill-buffer))
  "Q" 'evil-delete-buffer
  "o" 'find-file
  "l" 'imenu
  ";" 'eval-expression)

(evil-mode)
(evil-collection-init)

;; Which key should be activated after `evil-mode' since it detects evil mode
;; and changes some defaults. Could also just set
;; `which-key-allow-evil-operators' manually.
(customize-set-variable 'which-key-popup-type 'minibuffer)
(which-key-mode)

(provide 'my-keybindings)
;;; my-keybindings.el ends here
