;;; my-python-mode.el --- Python config -*- lexical-binding: t; -*-

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
;; Set up for Python mode.

;;; Code:

(require 'my-keybindings)
(require 'my-ui)

(autoload 'cython-mode "cython-mode")
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(defun my-python-imenu-setup ()
  (setq-local imenu-create-index-function 'treesit-simple-imenu)
  (setq-local treesit-simple-imenu-settings
	      `(("Cells" ,(rx "comment") my-python-cell-p my-python-cell-name)
		("Functions" ,(rx "function_definition")
		 (lambda (node) (not (my-python-method-p node))) nil)
		("Classes" ,(rx "class_definition") nil nil)
		("Methods" ,(rx "function_definition") my-python-method-p my-python-method-name)
		("Variables" ,(rx "assignment") my-python-global-var-p my-python-global-var-name)
		("Imports" ,(rx "import_" (? "from_") "statement") nil my-python-import-name))))

(with-eval-after-load 'python
  (require 'python-cell)

  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook
	    (lambda ()
	      (setq-local format-all-formatters '(("Python" ruff (ruff "check" "--fix-only"))))))
  (add-hook 'python-ts-mode-hook #'format-all-mode)
  (add-hook 'python-ts-mode-hook #'python-cell-mode)
  (add-hook 'python-ts-mode-hook #'my-python-imenu-setup)

  (customize-set-variable 'python-cell-highlight-cell nil)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(python-ts-mode "rass" "python")))

  (customize-set-variable 'python-indent-guess-indent-offset-verbose nil)
  (customize-set-variable 'python-shell-interpreter "ipython")
  (customize-set-variable 'python-shell-interpreter-args
			  "-i --simple-prompt --InteractiveShell.display_page=True")
  (customize-set-variable 'python-shell-completion-native-enable nil)

  (my-popper-add-reference "\\*pytest\\*.*")
  (my-popper-add-reference "\\*Python\\*")

  (general-nmap
    :keymaps 'python-ts-mode-map
    :prefix "C-c"
    "C-j" 'python-shell-send-statement
    "C-c" 'python-cell-shell-send-cell
    "C-f" 'python-shell-send-defun
    "C-b" 'python-shell-send-buffer)

  (general-vmap
    :keymaps 'python-ts-mode-map
    :prefix "C-c"
    "C-c" 'python-shell-send-region)

  (general-nvmap
    :keymaps 'python-ts-mode-map
    "M-n" 'python-cell-forward-cell
    "M-p" 'python-cell-backward-cell
    "C-M-n" 'python-nav-forward-block
    "C-M-p" 'python-nav-backward-block
    "C-M-a" 'python-nav-beginning-of-block
    "C-M-e" 'python-nav-end-of-block)

  (autoload 'python-pytest-dispatch "python-pytest")
  (my-local-leader-def
    :keymaps 'python-ts-mode-map
    "," 'run-python
    "t" 'python-pytest-dispatch)

  (defun my-wrap-run-python (fun &rest args)
    "Open python in project root instead of `default-directory' and return to
calling window."

    (let ((old-dir default-directory)
	  (win (selected-window)))
      (cd (projectile-project-root))
      (apply fun args)
      (select-window win)
      (cd old-dir)))

  (advice-add 'run-python :around #'my-wrap-run-python)

  (defun my-python-help (thing)
    "Open a help buffer for python THING."

    (interactive
     (list (let ((thing-candidate (python-eldoc--get-symbol-at-point)))
	     (read-string (concat
			   "Help on"
			   (if thing-candidate
			       (concat " (" thing-candidate ")")
			     "")
			   ": ")
			  nil
			  nil
			  thing-candidate))))
    (let* ((buff-name (format "*Python help for: %s*" thing))
	   (buff (get-buffer-create buff-name)))
      (with-current-buffer buff
	(insert
	 (python-shell-send-string-no-output (concat "help(" thing ")")))
	(python-mode)
	(goto-line 1))
      (switch-to-buffer-other-window buff)))

  ;; (defun my-python-imenu-format-item-label (type name)
  ;;   (format "ITEM %s: %s" type name))

  ;; (defun my-python-imenu-format-parent-item-label (type name)
  ;;   (format "PARENT %s: %s" type name))

  ;; (defun my-python-imenu-format-parent-item-jump-label (type name)
  ;;   (format "JUMP %s: %s" type name))

  ;; (setq python-imenu-format-item-label-function 'my-python-imenu-format-item-label)
  ;; (setq python-imenu-format-parent-item-label-function 'my-python-imenu-format-parent-item-label)
  ;; (setq python-imenu-format-parent-item-jump-label-function 'my-python-imenu-format-parent-item-jump-label)

  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
		 '(python-ts-mode :toplevel "Functions"
				  :types ((?f "Functions" font-lock-function-name-face)
					  (?c "Classes" font-lock-type-face)
					  (?i "Imports" font-lock-property-name-face)
					  (?m "Methods" font-lock-function-name-face)
					  (?v "Variables" font-lock-variable-name-face)
					  (?l "Cells" font-lock-comment-face)))))

  ;; REVIEW: Which help is better.
  (general-nmap
    :keymaps 'python-ts-mode-map
    :prefix "g"
    "?" 'my-python-help
    "K" 'python-describe-at-point))

(defun my-python-import-name (node)
  (let ((name (treesit-node-text
	       (treesit-node-child-by-field-name node "name")))
	(package (treesit-node-text
		  (treesit-node-child-by-field-name node "module_name"))))
    (if package
	(format "%s %s"
		(propertize (or package "") 'face 'font-lock-comment-face)
		name)
      name)))

(defun my-python-method-p (node)
  (let ((class-node
	 (treesit-parent-until node (lambda (item)
				      (string= (treesit-node-type item)
					       "class_definition")))))
    (treesit-node-text (treesit-node-child-by-field-name class-node "name"))))

(defun my-python-method-name (node)
  (let* ((class-node
	  (treesit-parent-until node (lambda (item)
				       (string= (treesit-node-type item)
						"class_definition"))))
	 (parent-name (treesit-node-text
		       (treesit-node-child-by-field-name class-node "name")))
	 (func-name (treesit-node-text
		     (treesit-node-child-by-field-name node "name"))))
    (format "%s %s"
	    (propertize (or parent-name "") 'face 'font-lock-comment-face)
	    func-name)))

(defun my-python-global-var-p (node)
  (let ((parent-node
	 (treesit-parent-until node (lambda (item)
				      (string= (treesit-node-type item)
					       "block")))))
    (message "Parent %s" parent-node)
    (not parent-node)))

(defun my-python-global-var-name (node)
  (let ((var-name
	 (treesit-node-text (treesit-node-child-by-field-name node "left")))
	(value-name
	 (treesit-node-text (treesit-node-child-by-field-name node "right"))))
    (format "%s = %s" var-name value-name)))

(defun my-python-cell-p (node)
  (s-starts-with-p "##" (treesit-node-text node)))

(defun my-python-cell-name (node)
  (let ((val (treesit-node-text node)))
    (string-clean-whitespace (s-chop-prefix "##" val))))


(provide 'my-python-mode)
;;; my-python-mode.el ends here
