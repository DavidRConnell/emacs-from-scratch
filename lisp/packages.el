;;; Declare packages installed packages here. -*- lexical-binding: t; -*-

(defvar straight-base-dir my-cache-dir)
(defvar bootstrap-version)
(defvar straight-check-for-modifications '(check-on-save find-when-checking))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

(provide 'packages)
;;; packages.el ends here
