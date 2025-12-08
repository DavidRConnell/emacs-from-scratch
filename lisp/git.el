;;; git.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: July 28, 2020
;; Modified: July 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/voidee/git
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
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
