;;; rss.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: July 28, 2020
;; Modified: July 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/voidee/rss
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

;; elfeed

(use-package elfeed
  :general
  (my-leader-def
    "e" #'elfeed)

  :config
  (general-define-key
   :keymaps 'elfeed-search-mode-map
   ;; "r" #'elfeed-update
   "&" #'elfeed-search-browse-url)

  (setq elfeed-search-filter "@2-week-ago")

  (set-face-attribute 'elfeed-search-title-face nil :weight 'light)
  (add-hook 'elfeed-show-mode-hook #'visual-line-mode)

  (use-package elfeed-org
    :init
    (setq rmh-elfeed-org-files (list "~/notes/elfeed.org"))
    :config
    (elfeed-org)))

(use-package elpher
  :commands elpher
  :config
  (general-nmmap
    :keymaps 'elpher-mode-map
    "C-o" #'elpher-back
    "C-c C-o" #'elpher-follow-current-link
    "C-n" #'elpher-next-link
    "C-p" #'elpher-prev-link)
  (setq elpher-start-page-url "gopher://gopher.floodgap.com/7/v2/vs"))

(provide 'rss)
;;; rss.el ends here
