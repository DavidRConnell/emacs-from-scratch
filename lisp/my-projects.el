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

(require 'project)

(autoload 'my-term "apps/my-terminal")

(dolist (fn '(projectile-switch-project
	      projectile-switch-open-project
	      projectile-switch-project-action
	      projectile-kill-buffers
	      projectile-compile-project
	      projectile-test-project
	      projectile-find-file
	      projectile-ripgrep
	      projectile-find-file-in-directory
	      projectile-project-p))
  (autoload fn "projectile" nil t))

(general-def
  :keymaps 'my-project-map
  "o" 'projectile-switch-project
  "O" 'projectile-switch-open-project
  "b" 'my-projectile-switch-buffer-other-project
  "w" 'projectile-save-project-buffers
  "-" 'project-dired
  "q" 'projectile-kill-buffers
  "c" 'projectile-compile-project
  "t" 'projectile-test-project)

(my-leader-def
  "SPC" #'projectile-find-file
  "," (defun my-open-term-in-project-or-dir ()
	"If in a projectile recognized directory open term in project root.
Otherwise open in `default-directory'."

	(interactive)
	(if (projectile-project-p)
	    (my-term (projectile-project-root))
	  (my-term)))

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
  (customize-set-variable 'projectile-completion-system 'default)
  (customize-set-variable 'projectile-cache-file
			  (expand-file-name "projects" my-cache-dir))
  (customize-set-variable ' projectile-git-submodule-command nil)
  (customize-set-variable ' projectile-git-use-fd nil)

  (projectile-cleanup-known-projects)

  (let ((prefix (expand-file-name "clones" (getenv "HOME"))))
    (dolist (dir (directory-files prefix 'full (rx bol (not "."))))
      (projectile-add-known-project dir)))

  (projectile-mode))

(provide 'my-projects)
;;; my-projects.el ends here
