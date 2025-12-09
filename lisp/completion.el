;;; completion.el --- Setup completion menus -*- lexical-binding: t -*-
;;
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
;; Completion menu setup. Provides general completion menu settings, requiring
;; more specific initialization files to make switching between completion
;; flexible.

;;; Code:

(use-package prescient
  :custom
  (prescient-aggressive-file-save t)
  (prescient-sort-length-enable nil)
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  (prescient-frequency-decay 0.997)
  (prescient-frequency-threshold 0.05)
  :config
  (prescient-persist-mode +1))

(require 'vertico-setup)

(use-package tempel
  :after (corfu cape)
  :disabled t
  :commands tempel-complete
  :general
  (general-imap
    "C-e" #'tempel-expand)
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
		      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (general-def
    :map tempel-map
    "M-n" #'tempel-next
    "M-p" #'tempel-previous
    "C-RET" #'tempel-done
    "C-c C-k" #'tempel-abort))

(use-package tempel-collection
  :load-path "/home/voidee/.config/emacs/vendor/tempel-collection"
  :after tempel
  :config
  (defun tempel-collection-reload ()
    (interactive)
    (setq tempel-collection--loaded nil)))

(use-package yasnippet
  :after yasnippet-capf
  :general
  (general-imap
    "C-e" #'yas-expand)
  :config
  (yas-global-mode t)
  (use-package doom-snippets
    :load-path "~/.cache/emacs/doom-snippets/"
    :config
    (setq doom-snippets-dir
	  (expand-file-name "doom-snippets/" my-cache-dir))
    (yas-reload-all))

  (general-define-key
   :keymaps 'yas-keymap
   "M-n" #'yas-next-field-or-maybe-expand
   "M-p" #'yas-prev-field)

  (add-to-list 'yas/snippet-dirs (expand-file-name "snippets"
						   user-emacs-directory))
  (yas-reload-all))

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'completion)
;;; completion.el ends here
