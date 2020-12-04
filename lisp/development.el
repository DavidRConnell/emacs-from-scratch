;;; development --- Setup development enviornment.
;;; Commentary:
;;   elisp development tools.  Watch JWiegley's videos
;;   some sort of eval program
;;   sly/lisp editing set-up.
;;   undo-tree
;;; Code:

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(use-package smartparens
  :config
  (setq smartparens-strict-mode t)
  (smartparens-global-mode)
  (require 'smartparens-config)
  (use-package evil-smartparens
    :delight :config
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

(use-package flycheck :hook (prog-mode . flycheck-mode)
             :config
             (setq flycheck-indication-mode 'right-fringe)
             (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
               [16 48 112 240 112 48 16] nil nil 'center))

(use-package emr
  :general (general-nmap "M-SPC" #'emr-show-refactor-menu))

(use-package elisp-def
  :hook (emacs-lisp-mode . elisp-def-mode))
;; :general (:keymaps 'elisp-def-mode-map "gd"))

;; (use-package elfmt
;;   :straight (elfmt :host github :repo "riscy/elfmt")
;;   :hook (emacs-lisp-mode . elfmt-mode))

(use-package highlight-function-calls
  :hook (emacs-lisp-mode . highlight-function-calls-mode)
  :config (use-package rainbow-identifiers
            :disabled :hook
            (prog-mode . rainbow-identifiers-mode)))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; (use-package elsa
;;   :mode "\\.el\\'"
;;   :config (use-package flycheck-elsa
;;   :config (add-hook 'emacs-lisp-mode-hook #'flycheck-elsa-setup)))

(use-package helpful
  :general (general-nmap :prefix "C-h"
                         "o" #'helpful-symbol
                         "k" #'helpful-key
                         "f" #'helpful-callable
                         "v" #'helpful-variable))

;; (use-package elisp-demos)
;; (use-package macrostep)
;; (use-package sly)

(use-package lispy
  :hook ((emacs-lisp-mode lisp-mode-hook)
         . lispy-mode)
  :config (use-package lispyville :hook (lispy-mode-hook . lispyville-mode)))

;; (use-package overseer)
;; (use-package buttercup)

(provide 'development)
;;; development.el ends here
