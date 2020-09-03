(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path
             (expand-file-name "lisp/" user-emacs-directory))

(require 'appearance)  ;; take care of general emacs stuff.
(require 'packages)    ;; declare packages
(require 'config)      ;; load and configure packages.

(gcmh-mode)

;; To-dos:
;; Get rid of menu bar
;; Stop custom from doing things
;; Add org-mode for better to-do support.
;; Finish setting up evil (see github)
;; Move package management out of init.el into packages.el (later move to nix)
;; stop from generating backup files.
;; speed up startup (it's already slow).
;; Packages:
;;   undo-tree
;;   file-snippets or whatever it is. Let's make sure keeping good formed files.
;;   elisp linter again start off on the right foot.
;;   spell-checker last one. Should work with magit.
;;   yas
;;   elisp development tools. Watch JWiegley's videos
;;   vterm
;;   projectile
;;   general   for binding keys also look into use-package's method might not be needed.
;;   persp?
;;   counsel/ivy
;;   which-key
;;   some sort of eval program
;;   sly/lisp editing set-up.


(provide 'init)
;;; init.el ends here
