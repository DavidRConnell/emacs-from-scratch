(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

(require 'packages)    ;; declare packages
(require 'appearance)  ;; take care of general emacs stuff.
(require 'config)      ;; load and configure packages.

(gcmh-mode)

;;; To-dos:
;; Add org-mode for better to-do support.
;; Finish setting up evil (see github)
;; Packages:

(provide 'init)
;;; init.el ends here
