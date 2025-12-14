;;; my-ess-mode.el --- Emacs Speaks Statistics -*- lexical-binding: t; -*-

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
;; Set up for various (aka R) ess modes.

;;; Code:

(require 'my-keybindings)
(require 'my-ui)

(autoload 'ess-r-mode "ess-r-mode")

;; In case I ever use another ess mode.
(with-eval-after-load 'ess-custom
  (customize-set-variable 'ess-offset-continued 'straight)
  (customize-set-variable 'ess-nuke-trailing-whitespace-p t)
  (customize-set-variable 'ess-style 'DEFAULT)
  (customize-set-variable 'ess-history-directory
			  (expand-file-name "ess-history/" my-var-dir)))

(with-eval-after-load 'ess-r-mode
  (require 'ess-custom)

  (add-hook ess-r-mode-hook #'format-all-mode)
  (my-popper-add-reference "\\*R:.*\\*")

  (general-def
    :keymaps '(ess-r-mode-map inferior-ess-r-mode-map)
    :prefix "C-c"
    "C-a" (lambda () (interactive)
	    (fixup-whitespace)
	    (insert " <- "))
    "C-p" (lambda (arg)
	    (interactive "P")
	    (if (not arg)
		(end-of-line))
	    (delete-horizontal-space)
	    (insert " |>"))))

(dolist (fn '(ess-eval-region-or-function-or-paragraph
	      ess-eval-region-or-function-or-paragraph-and-step
	      ess-eval-region
	      ess-eval-line-and-step))
  (autoload fn "ess-inf" nil t)

  (general-def
    :keymaps 'ess-r-mode-map
    :prefix "C-c"
    "C-c" '(ess-eval-region-or-function-or-paragraph :jump t)
    "c" 'ess-eval-region-or-function-or-paragraph-and-step
    "j" 'ess-eval-line-and-step)

  (my-local-leader-def
    :keymaps 'ess-r-mode-map
    "," 'R
    "t" (lambda ()
	  (interactive)
	  (ess-send-string (ess-get-process) (format "use_test(\"%s\")" (buffer-name))))
    "T" 'ess-r-devtools-test-package
    "c" 'ess-r-devtools-check-package
    "l" 'ess-r-devtools-load-package
    "u" 'ess-r-devtools-unload-package
    "b" 'ess-r-devtools-build
    "d" 'ess-r-devtools-document-package)

  (add-hook 'ess-help-mode-hook #'evil-motion-state)

  (autoload 'ess-help "ess-help" nil t)
  (autoload 'ess-display-vignettes "ess-help" nil t)
  (autoload 'ess-display-help-apropos "ess-help" nil t)
  (general-nmmap
    :keymaps '(ess-r-mode-map ess-help-mode-map)
    :prefix "g"
    "K" 'ess-help
    "V" 'ess-display-vignettes
    "o" 'ess-display-help-apropos)

  (defun my-goto-bottom-of-r-repl (&rest _)
    "Scroll to the bottom of the REPL after running a function.
This ensures the results are visible."
    (if (eq major-mode 'ess-r-mode)
	(save-selected-window
	  (ess-switch-to-end-of-ESS))))

  (advice-add #'ess-eval-region :after #'my-goto-bottom-of-r-repl))

(provide 'my-ess-mode)
;;; my-ess-mode.el ends here
