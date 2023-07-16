;;; vertico --- Setup vertico completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :config
  (general-define-key
   :keymaps 'vertico-map
   "C-n" #'vertico-next
   "C-p" #'vertico-previous
   "C-SPC" #'minibuffer-complete
   "C-w" #'backward-kill-word)
  (my-leader-def
    "o" #'find-file)
  (general-imap
    "C-SPC" #'completion-at-point)

  (setq vertico-cycle t)

  (vertico-mode 1))

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
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package consult
  :after vertico
  :config
  (require 'consult-imenu) ;; don't know why I need this now?
  (my-leader-def
    "b" #'consult-buffer
    "O" #'consult-locate
    "l" #'consult-imenu
    "L" #'consult-imenu-multi)
  ;; (general-nmmap
  ;;   "/" #'consult-line
  ;;   "gn" (defun consult-line-symbol-at-point ()
  ;; 	   (interactive)
  ;; 	   (consult-line (thing-at-point 'symbol))))
  )
(use-package cape
  :after corfu
  :config
  (defun my-cape-completion-generator (funcs)
    (let ((result))
      (dolist (element funcs result)
	(add-to-list 'completion-at-point-functions element 'append))))

  (add-hook 'text-mode-hook
	    (defun my-text-mode-capfs ()
	      (my-cape-completion-generator
	       (list #'cape-dict
		     #'cape-dabbrev
		     #'citar-capf
		     (cape-company-to-capf #'company-yasnippet)))))

  (add-hook 'prog-mode-hook (defun my-prog-mode-capfs ()
			      (my-cape-completion-generator
			       (list #'cape-dabbrev
				     #'cape-file
				     #'cape-keyword
				     (cape-company-to-capf #'company-yasnippet)))))

  (add-hook 'emacs-lisp-mode-hook (defun my-emacs-mode-capfs ()
				    (add-to-list 'completion-at-point-functions #'cape-symbol)))

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
   "C-i" #'embark-act)
  (general-define-key
   "C-h b" #'embark-bindings)
  (my-leader-def
    :infix "p"
    "g" #'consult-ripgrep))

(use-package embark-consult
  :after (embark consult)
  :config
  (general-def
    :keymaps 'embark-file-map
    "x" #'consult-file-externally
    "j" #'find-file-other-window)
  (general-def
    :keymaps 'embark-buffer-map
    "j" #'consult-buffer-other-window))

(use-package marginalia
  :after (consult vertico)
  :config
  (marginalia-mode 1)
  (general-define-key
   :keymaps 'minibuffer-local-map
   "M-a" #'marginalia-cycle))

(use-package flyspell-correct
  :general
  (general-nmap
    "z=" #'flyspell-correct-wrapper))

(provide 'vertico-setup)
;;; vertico-setup.el ends here
