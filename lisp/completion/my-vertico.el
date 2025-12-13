;;; my-vertico.el --- Setup vertico completion -*- lexical-binding: t -*-

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
;; Vertico related setup.

;;; Code:

(require 'my-keybindings)

(require 'vertico)
(require 'vertico-directory)
(require 'vertico-repeat)
(require 'vertico-multiform)
(require 'vertico-grid)
(require 'vertico-buffer)

(require 'consult)
(require 'consult-imenu)
(require 'consult-flymake)
(require 'consult-xref)

(require 'marginalia)

(general-def
  :keymaps 'vertico-map
  "C-n" #'vertico-next
  "C-p" #'vertico-previous
  "M-SPC" #'minibuffer-complete
  "M-RET" #'minibuffer-force-complete-and-exit
  "C-w" #'backward-kill-word)

(customize-set-variable 'vertico-cycle t)

(with-eval-after-load 'prescient
  (require 'vertico-prescient)
  (customize-set-variable 'vertico-prescient-enable-sorting t)
  (customize-set-variable 'vertico-prescient-override-sorting nil)
  (customize-set-variable 'vertico-prescient-enable-filtering nil) ;; deferred to orderless

  (vertico-prescient-mode))

(autoload 'vertico-quick-exit "vertico-quick")
(autoload 'vertico-quick-insert "vertico-quick")

(customize-set-variable 'vertico-quick1 "aoeu")
(customize-set-variable 'vertico-quick2 "snth")

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(general-def
  :keymaps 'vertico-map
  "C-j" 'vertico-directory-enter
  "C-w" 'vertico-directory-delete-word
  "DEL" 'vertico-directory-delete-char
  "C-s" 'vertico-quick-exit
  "M-s" 'vertico-quick-insert)

(general-nmmap
  :prefix "C-c"
  "C-r" #'vertico-repeat-last
  "R" #'vertico-repeat-select)

(customize-set-variable 'vertico-buffer-display-action
			'(display-buffer-in-direction
			  (direction . right)
			  (window-width . 0.26)))

(customize-set-variable 'vertico-multiform-commands
			'(
			  ;; Not sure if I Want these.
			  ;; (consult-imenu buffer)
			  ;; (consult-outline buffer)
			  ;; (consult-xref buffer)
			  (projectile-find-file grid)))

(customize-set-variable 'vertico-multiform-categories
			'((file grid)))

(vertico-multiform-mode)
(vertico-mode)

(global-set-key [remap switch-to-buffer] #'consult-buffer)
(global-set-key [remap project-switch-to-buffer] #'consult-project-buffer)
(global-set-key [remap imenu] #'consult-imenu)
(global-set-key [remap flymake-show-diagnostics-buffer] #'consult-flymake)
(global-set-key [remap recentf-open-files] #'consult-recent-file)

(my-leader-def
  "O" 'consult-fd
  "L" 'consult-imenu-multi
  "s" 'consult-outline)

(general-def
  :keymaps 'my-project-map
  "g" #'consult-ripgrep)

(general-nmmap
  :prefix "g"
  "n" (defun my-find-symbol ()
	(interactive)
	(consult-line (format "%s" (symbol-at-point)))))

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s" (replace-regexp-in-string
			      "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
			      crm-separator)
		(car args))
	(cdr args)))

(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(customize-set-variable 'consult-narrow-key "<")
(customize-set-variable 'xref-show-xrefs-function #'consult-xref)
(customize-set-variable 'xref-show-definitions-function #'consult-xref)

(dolist (fn '(embark-act
	      embark-act-all
	      embark-export
	      embark-select
	      embark-dwim
	      embark-bindings))
  (autoload fn "embark"))

(general-def
  :keymaps 'vertico-map
  "C-o" 'embark-act
  "C-a" 'embark-act-all
  "C-e" 'embark-export
  "C-SPC" 'embark-select)

(general-nmmap
  :keymaps 'override
  "<C-return>" 'embark-act)

(general-nmmap
  "RET" 'embark-dwim)

(global-set-key [remap describe-bindings] #'embark-bindings)

(with-eval-after-load 'embark
  (require 'embark-consult)
  (require 'wgrep)

  (general-def
    :keymaps 'grep-mode-map
    "i" 'wgrep-change-to-wgrep-mode)

  (general-def
    :keymaps 'embark-general-map
    "C-SPC" 'embark-cycle)

  (general-def
    :keymaps 'embark-file-map
    "x" 'embark-open-externally
    "j" 'find-file-other-window)

  (general-def
    :keymaps 'embark-buffer-map
    "j" 'consult-buffer-other-window))

(general-def
  :keymaps 'minibuffer-local-map
  "M-a" 'marginalia-cycle)

(marginalia-mode)

(provide 'my-vertico)
;;; my-vertico.el ends here
