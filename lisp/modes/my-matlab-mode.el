;;; my-matlab-mode.el --- MATLAB set up -*- lexical-binding: t; -*-

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
;; Configuration for matlab mode.

;;; Code:

(require 'my-keybindings)
(require 'my-ui)

(autoload 'matlab-mode "matlab")

;; Do not use matlab-ts-mode because the tree-sitter grammar is not installed.
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

(with-eval-after-load 'matlab
  (require 'matlab-shell)
  (require 'matlab-topic)

  (let ((load-path (append (list (expand-file-name "vendor/flymake-matlab"
						   user-emacs-directory))
			   load-path)))
    (require 'flymake-matlab))

  (add-hook 'matlab-mode-hook #'flymake-mode)
  (customize-set-variable 'flymake-matlab-backend 'auto)
  (add-hook 'matlab-mode-hook #'flymake-matlab-setup)

  ;; `matlab-mode' mode is not a derivative of `prog-mode'.
  (cl-loop for hk in prog-mode-hook
	   do (add-hook 'matlab-mode-hook hk))

  (my-popper-add-reference "\\*MATLAB\\*")

  (customize-set-variable 'matlab-indent-function-body t)
  (customize-set-variable 'matlab-functions-have-end t)

  (customize-set-variable 'matlab-shell-command-switches
			  '("-nodesktop" "-nosplash" "-softwareopengl"))
  (customize-set-variable 'matlab-shell-tab-use-company nil)
  (customize-set-variable 'matlab-fill-code t)
  (customize-set-variable 'matlab-highlight-cross-function-variables t)

  (defun my-wrap-matlab-shell (fun &rest args)
    "Open matlab in project root instead of `default-directory' and return to
calling window."

    (let ((old-dir default-directory)
	  (win (selected-window))
	  (buff (buffer-name)))
      (cd (project-root (project-current)))
      (apply fun args)
      (switch-to-buffer buff)
      (pop-to-buffer "*MATLAB*")
      (select-window win)
      (cd old-dir)))

  (advice-add #'matlab-shell :around #'my-wrap-matlab-shell)

  (general-nmmap
    :keymaps '(matlab-mode-map matlab-shell-mode-map matlab-shell-help-mode-map)
    "gK" (defun my-matlab-help-at-point ()
	   (interactive)
	   (matlab-shell-describe-command
	    (matlab-read-word-at-point)))
    "gD" 'matlab-shell-locate-fcn
    "gr" 'xref-find-references)

  (general-nvmap
    :keymaps 'matlab-mode-map
    :prefix "C-c"
    "C-c" 'matlab-shell-run-cell
    "C-b" 'matlab-shell-save-and-go
    "C-j" 'mt-shell-run-line
    "C-r" 'mt-shell-run-region)

  (defun my-matlab-capfs ()
    (setq-local completion-at-point-functions
		(list
		 (cape-company-to-capf #'company-matlab-shell)
		 #'semantic-analyze-completion-at-point-function
		 #'semantic-analyze-notc-completion-at-point-function
		 #'semantic-analyze-nolongprefix-completion-at-point-function
		 (cape-capf-super #'cape-keyword #'cape-dabbrev)
		 #'cape-file)

		cape-keyword-list '((matlab-mode
				     "break" "case" "catch" "classdef" "continue"
				     "else" "elseif" "end" "for" "function" "global"
				     "if" "otherwise" "parfor" "persistent" "return"
				     "spmd" "switch" "try" "while"))))

  (add-hook 'matlab-mode-hook #'my-matlab-capfs)
  (add-hook 'matlab-mode-hook #'semantic-mode)

  (add-hook 'matlab-shell-mode-hook
	    (lambda ()
	      (setq-local completion-at-point-functions
			  (list (cape-company-to-capf #'company-matlab-shell)
				#'cape-dabbrev
				#'cape-history))))

  (autoload 'mt-toggle-test-file "matlab-testing")
  (autoload 'mt-shell-run-tests "matlab-testing")
  (autoload 'mt-shell-run-performance-tests "matlab-testing")
  (autoload 'mt-run-command "matlab-testing")

  (defvar my-matlab-test-map (make-sparse-keymap))
  (defvar my-matlab-profile-map (make-sparse-keymap))
  (defvar my-matlab-gud-map (make-sparse-keymap))

  (my-local-leader-def
    :keymaps '(matlab-mode-map matlab-shell-mode-map)
    "t" '(:keymap my-matlab-test-map :which-key "test")
    "p" '(:keymap my-matlab-profile-map :which-key "profile")
    "d" '(:keymap my-matlab-gud-map :which-key "debug"))

  (general-def
    :keymaps 'my-matlab-test-map
    "g" 'mt-toggle-test-file
    "G" (lambda () (interactive)
	  (find-file (mt-get-test-dir)))
    "t" (lambda () (interactive)
	  (mt-shell-run-tests "file" "Unit"))
    "T" (lambda () (interactive)
	  (mt-shell-run-tests "project" "Unit"))
    "i" (lambda () (interactive)
	  (mt-shell-run-tests "project" "Integration"))
    "f" (lambda () (interactive)
	  (mt-shell-run-tests "project" "Functional"))
    "p" (lambda () (interactive)
	  (mt-shell-run-performance-tests "file"))
    "P" (lambda () (interactive)
	  (mt-shell-run-performance-tests "project"))
    "r" (lambda () (interactive)
	  (mt-shell-run-tests "rerun" "")))

  (general-def
    :keymaps 'my-matlab-profile-map
    "o" '(lambda () (interactive)
	   (mt-run-command "profile clear; profile on"))
    "s" '(lambda () (interactive)
	   (mt-run-command "profile off; profsave(profile('info'), '/tmp/matlab/profile_results')"))
    "v" '(lambda () (interactive)
	   (consult-file-externally "/tmp/matlab/profile_results/file0.html")))

  (dolist (fn '(mlgud-break
		mlgud-remove
		mlgud-up
		mlgud-down
		mlgud-step
		mlgud-next
		mlgud-until
		mlgud-cont
		mlgud-finish))
    (autoload fn "matlab-shell-gud" nil t))

  (general-def
    :keymaps 'my-matlab-gud-map
    "b" 'mlgud-break
    "r" 'mlgud-remove
    "f" 'mlgud-up
    "d" 'mlgud-down
    "s" 'mlgud-step
    "n" 'mlgud-next
    "u" 'mlgud-until
    "c" 'mlgud-cont
    "q" 'mlgud-finish)

  (autoload 'mozilla-readable "apps/my-eww")
  (my-local-leader-def
    :keymaps 'matlab-mode-map
    "," 'matlab-shell
    "r" (lambda (arg) (interactive "P")
	  (if arg
	      (call-interactively #'mt-run-command)
	    (call-interactively #'mt-run-last-command)))
    "?" (lambda ()
	  (interactive)
	  (let ((mathworks-ref-prefix "https://www.mathworks.com/help/matlab/ref/"))
	    (mozilla-readable (concat mathworks-ref-prefix (matlab-read-word-at-point) ".html")))))

  (defun my-matlab-insert-function-snippet ()
    "Add snippet for matlab function when opening a new .m file."
    (if (and (equal 0 (buffer-size))
	     (not (string-match-p "^\s*\\*.*\\*\s*$" (buffer-name))))
	(insert (concat "function " (file-name-sans-extension (buffer-name)) "\nend"))))

  (defun my-matlab-fix-imenu-generic-expression ()
    (setq-local imenu-generic-expression
		'(("Function" "^\\s-*\\(function\\)\\s-*\\([^\\.\n]*\\)" 2)
		  ("Function" "^\\s-*\\(function\\)\\s-*\\([^\\.\n]*\\.\\.\\.\\s-*\n.*\\)" 2)
		  ("Class" "^\\s-*\\(classdef\\)\\s-*\\(.*\\)" 2)
		  ("Arguments" "^\\s-*\\(arguments\\)\\s-*\\(.*\\)" 2)
		  ("Methods" "^\\s-*\\(methods\\)\\s-*\\(.*\\)?" 2)
		  ("Properties" "^\\s-*\\(properties\\)\\s-*\\(.*\\)?" 2)
		  ("Section" "^\\s-*%%\\s-*\\(.*\\)$" 1))))

  (add-hook 'matlab-mode-hook #'my-matlab-insert-function-snippet)
  (add-hook 'matlab-mode-hook #'my-matlab-fix-imenu-generic-expression)
  (add-hook 'matlab-mode-hook #'aggressive-indent-mode))

(provide 'my-matlab-mode)
;;; my-matlab-mode.el ends here
