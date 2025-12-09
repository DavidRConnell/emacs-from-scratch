;;; keybindings.el --- Evil key bindings -*- lexical-binding: t; -*-

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

;;; Code

(use-package general
  :config (general-evil-setup)
  (general-create-definer my-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer my-local-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "C-SPC")
  (general-create-definer general-nmmap
    :states '(normal motion))
  (general-create-definer general-nmvmap
    :states '(normal motion visual)))

(fset #'yes-or-no-p #'y-or-n-p)

(winner-mode t)
(use-package evil
  :init
  (setq evil-want-keybinding nil
	evil-search-module 'evil-search
	;; evil-undo-system 'undo-fu
	)
  :config
  (general-nmvmap
    :keymaps 'override
    "J" #'evil-scroll-line-down
    "K" #'evil-scroll-line-up
    "H" #'evil-beginning-of-visual-line
    "L" #'evil-end-of-line-or-visual-line
    "M" #'evil-goto-mark)
  (general-imap
    "C-u" #'evil-delete-back-to-indentation)
  (general-nmvmap
    "gj" #'evil-join
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "-" #'dired-jump
    "C-m" #'evil-goto-mark
    "C-q" #'evil-execute-macro)
  (general-nmmap
    :prefix "C-w"
    "u" #'winner-undo
    "C-r" #'winner-redo)
  (general-nmap
    :keymaps 'emacs-lisp-mode-map
    :prefix "C-c"
    "C-b" #'eval-buffer)
  (my-leader-def
    "b" #'switch-to-buffer
    "w" #'save-buffer
    "q" (lambda () (interactive) (kill-buffer))
    "Q" #'evil-delete-buffer
    "o" #'find-file
    "l" #'imenu
    ";" #'eval-expression)

  (advice-add 'push-mark :before (lambda (&rest _) (evil--jumps-push)))

  (evil-mode 1)

  (setq evil-echo-area-message nil
        evil-echo-state nil)

  (setq evil-ex-search-vim-style-regexp t)

  (use-package evil-collection
    :config (evil-collection-init))

  (use-package evil-nerd-commenter
    :commands (evilnc-comment-or-uncomment-lines evilnc-comment-operator)
    :general
    (general-nmap
      :prefix "g"
      "c" (general-key-dispatch
	      #'evilnc-comment-operator
	    "c" #'evilnc-comment-or-uncomment-lines)))

  (use-package evil-goggles
    :init (setq evil-goggles-duration 0.1
                evil-goggles-pulse nil
                evil-goggles-enable-change nil
                evil-goggles-enable-delete nil)
    :config (evil-goggles-mode))

  (use-package evil-surround
    :config (global-evil-surround-mode 1)))

(provide 'keybindings)
;;; keybindings.el ends here
