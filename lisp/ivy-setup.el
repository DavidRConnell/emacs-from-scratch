;;; ivy-setup --- Setup ivy completion -*- lexical-binding: t -*-

(use-package ivy
  :config
  (ivy-mode 1)
  (general-define-key
   :keymaps 'override
   "C-c C-r" #'ivy-resume)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-i" #'ivy-dispatching-done
   "C-w" #'ivy-backward-kill-word
   "C-SPC" #'ivy-mark)

  (use-package ivy-posframe
    :config
    (setq ivy-posframe-display-functions-alist
	  '((t . ivy-posframe-display-at-frame-center)))
    (setq ivy-posframe-parameters '((internal-border . 2)))
    (ivy-posframe-mode 1))

  (use-package ivy-avy
    :general
    (general-define-key
     :keymaps 'ivy-minibuffer-map
     "C-s" #'ivy-avy))

  (use-package ivy-rich
    :config
    (ivy-rich-mode 1))

  (use-package counsel
    :config
    (my-leader-def
      "O" #'counsel-recentf)
    (counsel-mode 1)
    (setq counsel-describe-function-function #'helpful-callable
	  counsel-describe-variable-function #'helpful-variable))

  (use-package orderless
    :config
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))
	  completion-styles '(orderless)))
  (savehist-mode)

  (use-package ivy-prescient
    :disabled
    :config
    (ivy-prescient-mode 1))

  (use-package swiper
    :general
    (general-nmmap
      "/" #'swiper
      "?" #'swiper-backward
      "gn" #'swiper-thing-at-point)
    :config
    (general-define-key
     :keymaps 'swiper-map
     "C-s" #'swiper-avy))

  (setq ivy-sort-max-size 7500))

(use-package amx
  :commands amx
  :init
  (setq amx-backend 'ivy)
  :config (amx-mode 1))

(provide 'ivy-setup)
