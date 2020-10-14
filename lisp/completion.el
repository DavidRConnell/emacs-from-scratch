;;   counsel/ivy
;;   company
;;   yas
;;   file-snippets or whatever it is. Let's make sure keeping good formed files.

(use-package ivy
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
