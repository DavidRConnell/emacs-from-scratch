;;; pass.el --- Use pass for authentication -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 David R. Connell
;;
;; Maintainer: David R. Connell <davidconnell12@gmail.com>
;; Created: January 15, 2023
;; Modified: January 15, 2023
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Set up auth-source to use my password-store.
;;; Code:

(setq auth-source-pass-filename
      (expand-file-name "password-store" (getenv "XDG_DATA_HOME")))

(auth-source-pass-enable)
(auth-source-forget-all-cached)

(provide 'pass)
;;; pass.el ends here
