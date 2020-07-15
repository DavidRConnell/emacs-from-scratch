(add-to-list 'load-path "~/projects/emacs-from-scratch.d/lisp")
(require 'my-packages)

;; use-package
(eval-when-compile
  (require 'use-package))

;; packages
(use-package evil
  :config
  (evil-mode 1))

(use-package magit)
(use-package evil-magit)
(use-package git-gutter
  :config
  (global-git-gutter-mode t))
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
