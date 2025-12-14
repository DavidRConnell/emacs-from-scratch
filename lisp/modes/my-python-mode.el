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

(autoload 'python-ts-mode "python-ts-mode")
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(with-eval-after-load 'python-ts-mode
  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'format-all-mode)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(python-ts-mode "pylsp")))

  (with-eval-after-load 'format-all
    (lambda () (setq-local format-all-formatters '(("Python" isort black)))))

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
    "C-c" 'python-shell-send-region
    "C-f" 'python-shell-send-defun
    "C-b" 'python-shell-send-buffer)

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

  ;; REVIEW: Which help is better.
  (general-nmap
    :keymaps 'python-ts-mode-map
    :prefix "g"
    "?" 'my-python-help
    "K" 'python-describe-at-point))

(provide 'my-python-mode)
;;; my-python-mode.el ends here
