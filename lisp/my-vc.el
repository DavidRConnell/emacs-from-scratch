;;; my-vc.el --- Version control -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Created: July 28, 2020

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
;; Version control setup.

;;; Code:

(require 'my-keybindings)

(require 'diff-hl)

(dolist (fn '(magit-status-here
	      magit-dispatch
	      magit-diff-buffer-file
	      magit-branch
	      magit-commit))
  (autoload fn "magit" nil t))

(general-def
  :keymaps 'my-vc-map
  "s" 'magit-status-here
  "g" 'magit-dispatch
  "d" 'magit-diff-buffer-file
  "b" 'magit-branch
  "c" 'magit-commit)

(with-eval-after-load 'magit
  (require 'magit-todos)

  (customize-set-variable 'magit-display-buffer-function
			  #'magit-display-buffer-fullframe-status-v1)

  (add-hook 'magit-mode-hook #'magit-todos-mode)

  (autoload 'forge-notifications-menu "forge")
  (autoload 'forge-topic-menu "forge")

  (with-eval-after-load 'forge
    (set forge-add-default-bindings nil)))

(with-eval-after-load 'git-rebase
  (general-def
    :keymaps 'git-rebase-mode-map
    "C-j" 'git-rebase-move-line-down
    "C-k" 'git-rebase-move-line-up))

(with-eval-after-load 'transient
  (general-def
    :keymaps 'transient-map
    "C-n" #'transient-scroll-up
    "C-p" #'transient-scroll-down))

(general-def
  :keymaps 'my-vc-map
  "r" 'diff-hl-revert-hunk
  "a" 'diff-hl-stage-current-hunk
  "n" '(diff-hl-next-hunk :jump t)
  "p" '(diff-hl-previous-hunk :jump t))

(customize-set-variable 'diff-hl-draw-borders nil)
(customize-set-variable 'diff-hl-show-staged-changes nil)
(customize-set-variable 'diff-hl-update-async t)
(customize-set-variable 'vc-git-diff-switches '("--histogram"))

(global-diff-hl-mode)

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(with-eval-after-load 'dired
  (require 'diff-hl-dired)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))

(autoload 'git-timemachine-toggle "git-timemachine" nil t)
(general-def
  :keymaps 'my-vc-map
  "t" 'git-timemachine-toggle)

(with-eval-after-load 'git-timemachine
  (general-def
    :keymaps 'git-timemachine-mode-map
    "C-j" 'git-timemachine-show-next-revision
    "C-k" 'git-timemachine-show-previous-revision))

(provide 'my-vc)
;;; my-vc.el ends here
