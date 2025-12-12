;;; projects --- Managing projects -*- lexical-binding: t; -*-

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

(autoload 'my-term "apps/my-terminal")

(defun my-projectile-switch-buffer-other-project ()
  "Switch to a buffer in an open project."
  (interactive)
  (let ((projectile-switch-project-action
	 #'(lambda () (consult-buffer '(consult--source-project-buffer)))))
    (projectile-switch-open-project)))

(use-package projectile
  :init
  (require 'f)
  :demand
  :general
  (my-leader-def
    :infix "p"
    "" '(:ignore t :which-key "project")
    "o" #'projectile-switch-project
    "O" #'projectile-switch-open-project
    "b" #'my-projectile-switch-buffer-other-project
    "w" #'projectile-save-project-buffers
    "-" #'project-dired
    "q" #'projectile-kill-buffers
    "c" #'projectile-compile-project
    "t" #'projectile-test-project)
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
  :config
  (projectile-cleanup-known-projects)
  (setq projectile-completion-system 'default
	projectile-cache-file (expand-file-name "projects" my-cache-dir)
	projectile-git-submodule-command nil
	projectile-git-use-fd nil)

  (let ((prefix (f-join (getenv "HOME") "clones")))
    (dolist (dir (directory-files prefix t))
      (unless (string-match-p (f-join prefix "\\.") dir)
	(projectile-add-known-project dir))))

  (projectile-mode +1))

(use-package project
  :disabled
  :general
  (my-leader-def
    :infix "p"
    "" '(:ignore t :which-key "project")
    "o" #'project-switch-project
    "-" #'project-dired
    "q" #'project-kill-buffers
    "b" #'project-switch-to-buffer)
  (my-leader-def
    "SPC" #'project-find-file
    "," (defun my-open-term-in-project-or-dir ()
	  "If in a projectile recognized directory open term in project root.
Otherwise open in `default-directory'."

	  (interactive)
	  (if (project-root)
	      (my-term (project-root))
	    (my-term)))

    "." (defun my-find-dot-file ()
	  "Find a file in `user-emacs-directory'."
	  (interactive)
	  (project-find-file-in "" "" user-emacs-directory)))
  :config
  (setq consult-project-root-function
	#'project-root))

(use-package eyebrowse
  :config
  (general-define-key
   :keymaps 'override
   :prefix "C-c C-w"
   "r" #'eyebrowse-rename-window-config
   "c" #'eyebrowse-create-window-config
   "b" #'eyebrowse-switch-to-window-config)
  (general-nmap
    :prefix "g"
    "t" #'eyebrowse-next-window-config
    "T" #'eyebrowse-prev-window-config)
  (general-nmap
    :prefix "z"
    "x" #'eyebrowse-close-window-config)
  (eyebrowse-mode t)
  (setq eyebrowse-wrap-around t
	eyebrowse-new-workspace t))

(use-package golden
  :straight (golden :type git :repo "https://git.sr.ht/~wklew/golden")
  :config (global-golden-mode 1))

(provide 'projects)
;;; projects.el ends here
