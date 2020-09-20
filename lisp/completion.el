;;   counsel/ivy
;;   company
;;   yas
;;   file-snippets or whatever it is. Let's make sure keeping good formed files.

(my-add-package company)

(my-add-package ivy)
(my-add-package counsel)
(my-add-package amx)
(my-add-package ivy-posframe)

(use-package ivy
  :config
  (setq ivy-mode 1)
  (setq ivy-sort-max-size 7500)
  (require 'counsel nil t))

(use-package counsel
  :defer t)

(use-package amx)
(use-package ivy-posframe
  :hook (ivy-mode . ivy-posframe-mode))

(use-package company)

(provide 'completion)
