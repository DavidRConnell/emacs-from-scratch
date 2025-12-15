;;; my-corfu.el --- Corfu CAPF -*- lexical-binding: t; -*-

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
;; Set up for CAPFs using Corfu and Cape.

;;; Code:
(require 'my-keybindings)

(require 'corfu)
(require 'corfu-info)
(require 'corfu-popupinfo)
(require 'corfu-echo)
(require 'corfu-prescient)

(require 'cape)
(require 'cape-keyword)

(customize-set-variable 'corfu-cycle t)
(customize-set-variable 'corfu-auto nil)
(customize-set-variable 'corfu-separator ?\s)
(customize-set-variable 'corfu-quit-at-boundary nil)
(customize-set-variable 'corfu-quit-no-match nil)
(customize-set-variable 'corfu-preview-current t)
(customize-set-variable 'corfu-preselect 'first)
(customize-set-variable 'corfu-on-exact-match nil)
(customize-set-variable 'corfu-min-width 25)

(general-def
  :keymaps 'corfu-map
  "C-n" 'corfu-next
  "C-p" 'corfu-previous
  "C-SPC" 'corfu-complete
  "M-s" 'corfu-move-to-minibuffer
  "M-g" 'corfu-info-location
  "M-h" 'corfu-info-documentation)

(autoload 'corfu-quick-complete "corfu-quick")
(customize-set-variable 'corfu-quick1 "aoeu")
(customize-set-variable 'corfu-quick2 "snth")

(general-def
  :keymaps 'corfu-map
  "C-s" 'corfu-quick-complete)

(evil-make-overriding-map corfu-map)

(corfu-echo-mode)
(corfu-popupinfo-mode)
(global-corfu-mode)

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
	completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
	      (bound-and-true-p vertico--input))
    (corfu-mode)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(customize-set-variable 'corfu-prescient-enable-sorting t)
(customize-set-variable 'corfu-prescient-override-sorting nil)
(customize-set-variable 'corfu-prescient-enable-filtering nil) ;; deferred to orderless

(corfu-prescient-mode)

(add-hook 'minibuffer-mode-hook
	  (defun my-minibuffer-mode-capfs ()
	    (setq-local completion-at-point-functions
			(list #'cape-dabbrev #'cape-history))))

(general-imap
  "C-x C-f" 'cape-file
  "C-x C-k" 'cape-dict)

(customize-set-variable 'cape-dict-file
			(list my-personal-dictionary my-alternate-dictionary))

;; Prefer ispell's capf to `cape-dict' since it matches from start of word
;; while `cape-dict' will find substrings in word.
(autoload 'ispell-completion-at-point "ispell" nil t)
(with-eval-after-load 'ispell
  (customize-set-variable 'ispell-personal-dictionary my-personal-dictionary)
  (customize-set-variable 'ispell-alternate-dictionary my-alternate-dictionary))

(provide 'my-corfu)
;;; my-corfu.el ends here
