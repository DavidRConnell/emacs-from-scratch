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
   "r" #'git-gutter:revert-hunk)
  :init
  (setq transient-history-file
        (expand-file-name "transient/history.el" my-var-dir))
  :config
  (advice-add 'magit-status :after #'delete-other-windows)
  (use-package evil-magit))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (use-package git-gutter-fringe)
  (setq-local git-gutter:init-function      #'git-gutter-fr:init
	      git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
	      git-gutter:clear-function     #'git-gutter-fr:clear
	      git-gutter:window-width -1)
  (fringe-mode '4)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(provide 'git)
;;; git.el ends here
