;;; git.el --- Version control -*- lexical-binding: t; -*-

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

(use-package vc
  :config
  (setq vc-follow-symlinks t))

(use-package magit
  :general
  (my-leader-def
    :infix "g"
    "s" #'magit-status-here
    "g" #'magit-dispatch
    "d" #'magit-diff-buffer-file
    "b" #'magit-branch
    "c" #'magit-commit)
  :config
  (require 'magit-branch)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (general-define-key
   :keymaps 'git-rebase-mode-map
   "C-j" #'git-rebase-move-line-down
   "C-k" #'git-rebase-move-line-up))

(use-package forge
  :after magit
  :config
  (setq evil-collection-forge-setup nil))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package transient
  :config
  (general-def
    :keymaps 'transient-map
    "C-n" #'transient-scroll-up
    "C-p" #'transient-scroll-down)
  ;; (setq transient-display-buffer-action
  ;; 	'(display-buffer-in-side-window
  ;; 	  (side . top)
  ;; 	  (dedicated . t)
  ;; 	  (inhibit-same-window . t)
  ;; 	  (window-parameters (no-other-window . t))))
  )

(use-package git-gutter-fringe)
(use-package git-gutter
  :after git-gutter-fringe
  :config
  (my-leader-def
    :infix "g"
    "r" #'git-gutter:revert-hunk
    "a" #'git-gutter:stage-hunk
    "n" #'git-gutter:next-hunk
    "p" #'git-gutter:previous-hunk)

  (global-git-gutter-mode t)
  (setq-local git-gutter:init-function      #'git-gutter-fr:init
	      git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
	      git-gutter:clear-function     #'git-gutter-fr:clear
	      git-gutter:window-width -1)
  (fringe-mode '4)
  (define-fringe-bitmap 'git-gutter-fr:added [128]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [128]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(use-package git-timemachine
  :general
  (my-leader-def
    :infix "g"
    "t" #'git-timemachine-toggle)
  :config
  (general-define-key
   :keymaps 'git-timemachine-mode-map
   "C-k" #'git-timemachine-show-previous-revision
   "C-j" #'git-timemachine-show-next-revision))

(provide 'git)
;;; git.el ends here
