;;; my-prog-mode.el --- General programming mode set up -*- lexical-binding: t; -*-

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
;; Set up general programming. As with `my-text-mode', includes some simpler
;; specific modes.

;;; Code:

(require 'my-keybindings)
(require 'my-ui)
(require 'my-completion)

(defvar my-gud-map (make-sparse-keymap))

(general-def
  :keymaps 'my-gud-map
  "d" 'gud-gdb
  "b" 'gud-break
  "r" 'gud-remove
  "f" 'gud-up
  "s" 'gud-step
  "n" 'gud-next
  "u" 'gud-until
  "c" 'gud-cont
  "l" 'gud-refresh
  "p" 'gud-print
  "q" 'gud-finish)

(add-hook 'prog-mode-hook
	  (defun my-prog-mode-capfs ()
	    (setq-local completion-at-point-functions
			(list #'pcomplete-completions-at-point
			      #'cape-dabbrev #'cape-keyword #'cape-file))))

(require 'ansi-color)
(defun my-colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(my-popper-add-reference 'compilation-mode)
(add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)
(customize-set-variable 'compilation-scroll-output 'first-error)
(customize-set-variable 'compilation-auto-jump-to-first-error nil)

;; Turn off checking tree-sitter since it is managed by Nix.
(customize-set-variable 'tsc-dyn-get-from '())

(require 'direnv)
(add-to-list 'auto-mode-alist '("\\.envrc'" . direnv-envrc-mode))
(add-to-list 'auto-mode-alist '("\\direnvrc'" . direnv-envrc-mode))
(direnv-mode)

;; REVIEW: Do I need both flycheck and flymake?
(use-package flycheck
  :disabled t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(autoload 'manual-entry "man" nil t)

(with-eval-after-load 'man
  (general-def
    :keymaps Man-mode-map
    "gK" 'my-man-at-point))

;; REVIEW: Not sure if I want this to be a pop up or regular?
;; If a pop up, possibly wrap manual call in `save-selected-window'.
;; (my-popper-add-reference 'Man-mode)
(defun my-man-at-point (arg &optional section)
  (interactive "P")
  (if arg
      (call-interactively #'manual-entry)
    (let* ((base-command (Man-default-man-entry))
	   (command (if section
			(format "%d %s" section base-command)
		      (format "-a %s" base-command)))
	   (buff-name (format "*Man %s*" command)))
      (manual-entry command))))

(require 'smartparens)
(require 'smartparens-config)
(require 'evil-smartparens)

(setq-default smartparens-strict-mode t)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(smartparens-global-mode)

(autoload 'mhtml-mode "mhtml-mode")
(add-to-list 'auto-mode-alist '("\\.html" . mhtml-mode))

(with-eval-after-load 'mhtml-mode
  (add-hook 'mhtml-mode-hook
	    (lambda ()
	      (setq-local format-all-formatters '(("HTML" prettierd)))))
  (add-hook 'mhtml-mode-hook #'format-all-mode))

(autoload 'gnuplot-mode "gnuplot-mode")
(add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))

(autoload 'mermaid-mode "mermaid-mode")
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

(autoload 'dockerfile-ts-mode "dockerfile-ts-mode")
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\*.docker\\'" . dockerfile-ts-mode))

(general-def
  :keymaps 'Info-mode-map
  "C-c C-o" 'Info-follow-nearest-node)

(provide 'my-prog-mode)
;;; my-prog-mode.el ends here
