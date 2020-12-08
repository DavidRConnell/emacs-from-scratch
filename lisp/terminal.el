;;; terminal.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <http://github/voidee>
;; Maintainer: David R. Connell <voidee@TheVoid>
;; Created: July 28, 2020
;; Modified: July 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/voidee/terminal
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(use-package vterm
  :straight nil
  :general
  (my-leader-def
    "," #'vterm)
  (general-imap
    :keymaps 'vterm-mode-map
    "C-SPC" #'vterm-send-tab
    "C-y" #'vterm-yank))

(provide 'terminal)
;;; terminal.el ends here
