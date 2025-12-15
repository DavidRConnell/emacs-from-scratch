;;; my-emacs-lisp-mode.el --- elisp set up -*- lexical-binding: t; -*-

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
;; Configure Emacs Lisp editing.

;;; Code:

(require 'my-ui)
(require 'my-keybindings)
(require 'my-completion)
(require 'my-lisp-mode "modes/my-lisp-mode")

(require 'elisp-def)
(require 'highlight-function-calls)
(require 'highlight-defined)

(add-hook 'emacs-lisp-mode-hook #'elisp-def-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-function-calls-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(add-hook 'emacs-lisp-mode-hook #'format-all-mode)

(add-hook 'emacs-lisp-mode-hook
	  (defun my-elisp-mode-add-capfs ()
	    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
	    (add-to-list 'completion-at-point-functions #'elisp-completion-at-point)))

(general-nmap
  :keymaps 'emacs-lisp-mode-map
  :prefix "C-c"
  "C-b" 'eval-buffer)

(dolist (fn '(helpful-symbol
	      helpful-key
	      helpful-callable
	      helpful-variable))
  (autoload fn "helpful" nil t))

(general-def
  [remap describe-symbol] 'helpful-symbol
  [remap describe-key] 'helpful-key
  [remap describe-function] 'helpful-callable
  [remap describe-variable] 'helpful-variable)

(with-eval-after-load 'helpful
  (require 'elisp-demos)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (let ((require-regex
		   (rx "(require '" (group (* (not space))) (? (* (not ")"))) ")")))
	      (add-to-list 'imenu-generic-expression
			   `("Package" ,require-regex 1)))))

(provide 'my-emacs-lisp-mode)
;;; my-emacs-lisp-mode.el ends here
