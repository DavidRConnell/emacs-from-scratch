(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path "~/projects/emacs-from-scratch.d/lisp")
(require 'my-packages)

;; use-package
(eval-when-compile
  (require 'use-package))

;; packages
(use-package gcmh
  :config
  (setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 16 1024 1024)))  ; 16mb

(use-package evil
  :config
  (evil-mode 1))

(use-package magit)
(use-package evil-magit)
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
