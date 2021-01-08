;;; completion --- Setup completion types -*- lexical-binding: t -*-
;;   counsel/ivy
;;   company
;;   yas
;;   file-snippets or whatever it is. Let's make sure keeping good formed files.
;;; Commentary:
;;; Code:

(use-package prescient
  :init (setq prescient-save-file (expand-file-name
                                   "prescient-save.el" my-var-dir))
  :config
  (prescient-persist-mode +1))

(use-package selectrum
  :disabled
  :config
  (general-define-key
   :keymaps 'global
   "C-c C-r" #'selectrum-repeat)
  (general-define-key
   :keymaps 'selectrum-minibuffer-map
   "C-w" #'evil-delete-backward-word
   "C-u" #'evil-delete-back-to-indentation)

  (setq selectrum-num-candidates-displayed 10)
  (selectrum-mode +1)

  (use-package selectrum-prescient
    :after prescient
    :config
    (selectrum-prescient-mode +1))

  (use-package consult
    :straight '(consult :type git :host github :repo "minad/consult")
    :general
    (general-define-key
     :states '(normal visual motion)
     :jump t
     "/" #'consult-line
     "gn" #'consult-line-symbol-at-point)
    (my-leader-def
      :jump t
      "b" #'consult-buffer
      "C-b" #'consult-buffer-other-window
      "M-b" #'consult-buffer-other-frame)))

(use-package mini-frame
  :disabled
  :init (setq mini-frame-show-parameters
	      '((top . 0.33) (width . 0.7) (left . 0.5)))
  :config
  (mini-frame-mode +1))

(use-package ctrlf
  :disabled
  :general (general-nmap
	     "/" #'ctrlf-forward-fuzzy-regexp
	     "?" #'ctrlf-backward-fuzzy-regexp
	     "g*" #'ctrlf-forward-symbol-at-point)
  :config
  (ctrlf-mode +1))

(use-package ivy
  :config
  (ivy-mode 1)
  (general-define-key
   :keymaps 'override
   "C-c C-r" #'ivy-resume)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-i" #'ivy-dispatching-done
   "C-w" #'ivy-backward-kill-word)

  (use-package ivy-prescient
    :config
    (ivy-prescient-mode 1))

  (use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
	'((t .  ivy-posframe-display-at-frame-center)))
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
    (counsel-mode 1))

  (use-package swiper
    :general
    (general-nmmap
      "/" #'swiper-isearch
      "?" #'swiper-isearch-backward
      "gn" #'swiper-isearch-thing-at-point)
    :config
    (general-define-key
     :keymaps 'swiper-map
     "C-s" #'swiper-avy))

  (setq ivy-sort-max-size 7500))

(use-package amx
  :commands amx
  :init
  (setq amx-save-file (expand-file-name "amx-items" my-var-dir)
	amx-backend 'ivy)
  :config (amx-mode 1))

(use-package company
  :config
  (general-define-key
   :keymaps 'company-active-map
   "C-w" nil
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "C-h" #'company-show-doc-buffer
   "C-s" #'company-filter-candidates
   "C-i" #'company-complete-selection
   "C-SPC" #'company-complete-common-or-cycle)

  (general-imap
    "C-SPC" #'company-complete
    "C-n" #'company-dabbrev
    "C-f" #'company-files
    "C-s" #'company-ispell)

  (general-define-key
   :keymaps 'company-search-map
   "C-n" #'company-select-next-or-abort
   "C-p" #'company-select-previous-or-abort)

  (global-company-mode 1)

  (add-hook 'text-mode-hook (defun my-set-text-mode-backends ()
			      (setq-local company-backends
					  '(company-dabbrev))))

  (setq company-idle-delay 0.2
	company-minimum-prefix-length 2
	company-selection-wrap-around t)

  (use-package company-quickhelp
    :disabled
    :hook (company-mode . company-quickhelp-mode))

  (use-package company-box
    :hook (company-mode . company-box-mode)
    :config (setq company-box-scrollbar nil))

  (use-package company-prescient
    :hook (company-mode . company-prescient-mode))

  (use-package company-math
    :general
    (general-imap
      "C-\\" #'company-math-symbols-unicode)))

(use-package yasnippet
  :hook ((text-mode prog-mode snippet-mode) .
	 yas-minor-mode-on)
  :general
  (general-imap
    "C-e" #'company-yasnippet)
  :config
  (use-package doom-snippets
    :straight nil
    :load-path "~/.cache/emacs/doom-snippets/"
    :config
    (setq doom-snippets-dir
	  (expand-file-name "doom-snippets/" my-cache-dir))
    (yas-reload-all))

  (general-define-key
   :keymaps 'yas-keymap
   "C-SPC" #'yas-next-field-or-maybe-expand)
  (defun my-add-yasnippet-backend ()
    (add-to-list 'company-backends #'company-yasnippet 'append))

  (add-hook 'yas-minor-mode-hook #'my-add-yasnippet-backend))



(provide 'completion)
