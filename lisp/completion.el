;;   counsel/ivy
;;   company
;;   yas
;;   file-snippets or whatever it is. Let's make sure keeping good formed files.

(use-package prescient
  :config
  (prescient-persist-mode +1))

(use-package selectrum
  :config
  (use-package selectrum-prescient)
  (selectrum-mode +1)
  (selectrum-prescient-mode +1))

(use-package mini-frame
  :config
  (mini-frame-mode +1)
  (setq mini-frame-show-parameters '((top . 0.25) (width . 0.7) (left . 0.5))))

(use-package ctrlf
  :config
  (ctrlf-mode +1))

(use-package ivy
  :disabled
  :config
  (use-package ivy-posframe
    :hook (ivy-mode . ivy-posframe-mode))
  (use-package counsel)
  (setq ivy-mode 1)
  (setq ivy-sort-max-size 7500)
  (require 'counsel nil t))

(use-package amx
  :config (amx-mode 1))

(use-package company
  :general
  (general-imap "C-SPC" #'company-complete)
  :config
  (global-company-mode)
  (use-package company-prescient))

(provide 'completion)
