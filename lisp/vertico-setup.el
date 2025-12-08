;;; vertico --- Setup vertico completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :demand t
  :config
  (general-define-key
   :keymaps 'vertico-map
   "C-n" #'vertico-next
   "C-p" #'vertico-previous
   "M-SPC" #'minibuffer-complete
   "M-RET" #'minibuffer-force-complete-and-exit
   "C-w" #'backward-kill-word)
  (my-leader-def
    "o" #'find-file)
  (general-imap
    "C-SPC" #'completion-at-point)

  (setq vertico-cycle t)

  (vertico-mode 1))

(use-package vertico-prescient
  :demand t
  :after vertico prescient
  :custom
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil)
  (vertico-prescient-enable-filtering nil) ;; deferred to orderless
  :config
  (vertico-prescient-mode t))

(use-package vertico-directory
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :general
  (general-def
    :keymaps 'vertico-map
    "C-j" #'vertico-directory-enter
    "C-w" #'vertico-directory-delete-word
    "DEL" #'vertico-directory-delete-char))

(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :general
  (general-nmmap
    :prefix "C-c"
    "C-r" #'vertico-repeat-last
    "R" #'vertico-repeat-select))

(use-package vertico-quick
  :after vertico
  :general
  (general-define-key
   :keymaps 'vertico-map
   "C-s" #'vertico-quick-exit
   "M-s" #'vertico-quick-insert)
  :config
  (setq vertico-quick1 "aoeu"
	vertico-quick2 "snth"))

(use-package vertico-buffer
  :after vertico-multiform
  :config
  (setq vertico-buffer-display-action
	'(display-buffer-in-direction
          (direction . right)
          (window-width . 0.26)))
  (vertico-buffer-mode -1))

(use-package vertico-grid
  :after vertico-multiform)

(use-package vertico-multiform
  :after vertico
  :config
  (setq vertico-multiform-commands
	'((consult-imenu buffer)
	  (consult-outline buffer)
	  (consult-xref buffer)
	  (projectile-find-file grid)))
  (setq vertico-multiform-categories
	'((file grid)))
  (vertico-multiform-mode t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :demand
  :config
  (require 'consult-imenu)
  (require 'consult-flymake)
  (require 'consult-xref)
  (my-leader-def
    "b" #'consult-buffer
    "O" #'consult-locate
    "e" #'consult-flymake
    "l" #'consult-imenu
    "L" #'consult-imenu-multi
    "s" #'consult-outline)
  (general-nmmap
    :prefix "g"
    "n" (defun my-find-symbol ()
	  (interactive)
	  (consult-line (format "%s" (symbol-at-point)))))

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq consult-narrow-key "<")
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

(use-package cape
  :after corfu
  :config
  (require 'citar-capf)
  (require 'cape-keyword)

  (defun my-cape-completion-generator (funcs)
    (let ((result))
      (dolist (element funcs result)
	(add-to-list 'completion-at-point-functions element 'append))))

  (add-hook 'text-mode-hook
	    (defun my-text-mode-capfs ()
	      (my-cape-completion-generator
	       (list #'cape-dict
		     #'cape-dabbrev
		     #'citar-capf))))

  (add-hook 'prog-mode-hook (defun my-prog-mode-capfs ()
			      (my-cape-completion-generator
			       (list #'cape-dabbrev
				     #'cape-file
				     #'cape-keyword))))

  (add-hook 'emacs-lisp-mode-hook (defun my-emacs-mode-capfs ()
				    (add-to-list 'completion-at-point-functions
						 #'cape-elisp-symbol)))

  (add-hook 'minibuffer-mode-hook (defun my-minibuffer-mode-capfs ()
				    (setq-local completion-at-point-functions
						(list #'cape-dabbrev #'cape-history))))

  (general-imap
    "C-x C-f" #'cape-file
    "C-x C-k" #'cape-dict)

  (setq cape-dict-file (list ispell-personal-dictionary ispell-alternate-dictionary))
  ;; WARNING: May cause performance issues. If eglot slow remove and look at
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot for other
  ;; options.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package embark
  :after consult
  :config
  (general-define-key
   :keymaps 'vertico-map
   "C-o" #'embark-act
   "C-a" #'embark-act-all
   "C-e" #'embark-export
   "C-SPC" #'embark-select)
  (general-define-key
   [remap describe-bindings] #'embark-bindings)
  (general-nmmap
    :keymaps 'override
    "<C-return>" #'embark-act)
  (general-nmmap
    "RET" #'embark-dwim))

(use-package wgrep
  :after embark-consult
  :config
  (general-def
    :keymaps 'grep-mode-map
    "i" #'wgrep-change-to-wgrep-mode))

(use-package embark-consult
  :after consult
  :config
  (general-def
    :keymaps 'embark-general-map
    "C-SPC" #'embark-cycle)
  (my-leader-def
    :infix "p"
    "g" #'consult-ripgrep)
  (general-def
    :keymaps 'embark-file-map
    "x" #'embark-open-externally
    "j" #'find-file-other-window)
  (general-def
    :keymaps 'embark-buffer-map
    "j" #'consult-buffer-other-window))

(use-package marginalia
  :after consult
  :config
  (marginalia-mode)
  (general-define-key
   :keymaps 'minibuffer-local-map
   "M-a" #'marginalia-cycle))

(use-package flyspell-correct
  :general
  (general-nmap
    "z=" #'flyspell-correct-wrapper))

(use-package corfu
  :demand t
  :general
  (general-imap
    "C-SPC" #'completion-at-point)
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preview-current t)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-min-width 25)
  :config
  (general-def
    :keymaps 'corfu-map
    "C-n" #'corfu-next
    "C-p" #'corfu-previous
    "C-SPC" #'corfu-complete
    "M-s" #'corfu-move-to-minibuffer)

  (use-package corfu-history
    :disabled t
    :after corfu
    :config
    (corfu-history-mode t)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  (use-package corfu-quick
    :custom
    (corfu-quick1 "aoeu")
    (corfu-quick2 "snth")
    :general
    (general-def
      :keymaps 'corfu-map
      "C-s" #'corfu-quick-complete))

  (use-package corfu-info
    :general
    (general-def
      :keymaps 'corfu-map
      "M-g" #'corfu-info-location
      "M-h" #'corfu-info-documentation))

  (use-package corfu-popupinfo
    :after corfu
    :config
    (corfu-popupinfo-mode t))

  (use-package corfu-echo
    :after corfu
    :config
    (corfu-echo-mode t))

  (evil-make-overriding-map corfu-map)
  (global-corfu-mode)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
	  completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input))
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  ;; (general-def
  ;;   :keymaps 'evil-ex-map
  ;;   "M-p" #'completion-at-point)
  )

(use-package corfu-prescient
  :demand t
  :after corfu prescient
  :custom
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting nil)
  (corfu-prescient-enable-filtering nil) ;; deferred to orderless
  :config
  (corfu-prescient-mode t))

(provide 'vertico-setup)
;;; vertico-setup.el ends here
