;;; my-projects.el --- Managing projects -*- lexical-binding: t; -*-

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
;; Project level functions.

;;; Code:

(require 'my-keybindings)
(require 'my-variables)

(setq projectile-completion-system 'default
      projectile-cache-file (expand-file-name "projects" my-cache-dir)
      projectile-git-submodule-command nil
      projectile-git-use-fd nil
      projectile-test-prefix-function #'my-projectile-test-prefix)

(defun my-projectile-test-prefix (project-type)
  "Find default test files prefix based on PROJECT-TYPE."
  (projectile-project-type-attribute project-type 'test-prefix "test_"))

(require 'project)

(autoload 'my-term "apps/my-terminal")

(dolist (fn '(projectile-switch-project
	      projectile-switch-open-project
	      projectile-switch-project-action
	      projectile-kill-buffers
	      projectile-compile-project
	      projectile-test-project
	      projectile-find-file
	      projectile-project-root
	      projectile-ripgrep
	      projectile-find-file-in-directory
	      projectile-project-type-attribute
	      projectile-project-p))
  (autoload fn "projectile" nil t))

(general-def
  :keymaps 'my-project-map
  "o" 'projectile-switch-project
  "O" 'projectile-switch-open-project
  "b" 'project-switch-to-buffer
  "w" 'projectile-save-project-buffers
  "-" 'project-dired
  "q" 'project-kill-buffers
  "c" 'projectile-compile-project
  "t" 'projectile-test-project)

(my-leader-def
  "SPC" 'project-find-file
  "," (defun my-open-term-in-project-or-dir ()
	"If in a projectile recognized directory open term in project root.
Otherwise open in `default-directory'."

	(interactive)
	(my-term (projectile-project-root)))

  "." (defun my-find-dot-file ()
	"Find a file in `user-emacs-directory'."
	(interactive)
	(projectile-find-file-in-directory user-emacs-directory)))

(defun my-projectile-switch-buffer-other-project ()
  "Switch to a buffer in an open project."
  (interactive)
  (let ((projectile-switch-project-action
	 #'(lambda () (consult-buffer '(consult--source-project-buffer)))))
    (projectile-switch-open-project)))

(with-eval-after-load 'projectile
  (projectile-cleanup-known-projects)

  (let ((parent (expand-file-name "clones" "~/")))
    (when (file-directory-p parent)
      (dolist (dir (directory-files parent 'full (rx bol (not "."))))
	(projectile-add-known-project dir))))

  (projectile-mode))

(provide 'my-projects)
;;; my-projects.el ends here
