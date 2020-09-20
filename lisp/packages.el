;;; Declare packages installed packages here.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package use-package)
(eval-when-compile
  (require 'use-package))

;; (my-add-package helpful)
;; (my-add-package elisp-demos)
;; (my-add-package elisp-def)
;; (my-add-package macrostep)
;; (my-add-package sly)
;; (my-add-package lispy)
;; (my-add-package lispyville)
;; (my-add-package overseer)
;; (my-add-package buttercup)

(provide 'packages)
