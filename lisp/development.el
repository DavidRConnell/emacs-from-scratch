;;   elisp linter again start off on the right foot.
;;   elisp development tools. Watch JWiegley's videos
;;   some sort of eval program
;;   sly/lisp editing set-up.
;;   undo-tree

(use-package smartparens
  :config
  (setq smartparens-strict-mode t)
  (smartparens-global-mode)
  (use-package evil-smartparens
    :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))


;; (use-package helpful)
;; (use-package elisp-demos)
;; (use-package elisp-def)
;; (use-package macrostep)
;; (use-package sly)
;; (use-package lispy)
;; (use-package lispyville)
;; (use-package overseer)
;; (use-package buttercup)

(provide 'development)
