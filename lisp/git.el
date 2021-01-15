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

(use-package magit
  :general
  (my-leader-def
   :infix "g"
   "s" #'magit-status-here
   "d" #'magit-diff-buffer-file
   "b" #'magit-branch
   "r" #'git-gutter:revert-hunk
   "a" #'git-gutter:stage-hunk
   "c" #'magit-commit)
  :config
  (advice-add 'magit-status :after #'delete-other-windows)
  (general-define-key
   :keymaps 'git-rebase-mode-map
   "C-j" #'git-rebase-move-line-down
   "C-k" #'git-rebase-move-line-up))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (use-package git-gutter-fringe)
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
    "t" #'git-timemachine)
  :config
  (general-define-key
   :keymaps 'git-timemachine-mode-map
   "C-j" #'git-timemachine-show-previous-revision
   "C-k" #'git-timemachine-show-next-revision
   "C-r" #'git-timemachine-show-revision))

(provide 'git)
;;; git.el ends here
