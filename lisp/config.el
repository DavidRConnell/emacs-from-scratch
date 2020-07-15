;;; Load in and configure packages here.

;; use-package setup
(eval-when-compile
  (require 'use-package))

;; package configuration
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

(provide 'config)
