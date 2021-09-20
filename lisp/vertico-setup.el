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
  (setq completion-in-region-function #'consult-completion-in-region)

  (use-package vertico-directory
    :load-path "~/.cache/emacs/vertico/extensions"
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    :config
    (general-define-key
     :keymaps 'vertico-map
     "C-j" #'vertico-directory-enter
     "C-w" #'vertico-directory-delete-word
     "DEL" #'vertico-directory-delete-char))

  (use-package vertico-repeat
    :load-path "~/.cache/emacs/vertico/extensions"
    :general
    (general-nmmap
      "C-c C-r" #'vertico-repeat))

  (use-package vertico-reverse
    :disabled
    :load-path "~/.cache/emacs/vertico/extensions"
    :after consult
    :general
    (general-nmmap
      "?" (defun consult-line-reverse ()
	    (interactive)
	    (vertico-reverse-mode 1)
	    (consult-line)
	    (vertico-reverse-mode -1))))

  (use-package vertico-buffer
    :disabled
    :load-path "~/.cache/emacs/vertico/extensions"
    :config
    (vertico-buffer-mode 1))

  (use-package vertico-quick
    :load-path "~/.cache/emacs/vertico/extensions"
    :general
    (general-define-key
     :keymaps 'vertico-map
     "C-s" #'vertico-quick-exit
     "M-s" #'vertico-quick-insert)
    :config
    (setq vertico-quick1 "aoeu"
	  vertico-quick2 "snth"))

  (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package embark
  :after consult
  :config
  (general-define-key
    :keymaps 'vertico-map
    "C-i" #'embark-act
    "C-j" #'embark-dwim)
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
  :config
  (marginalia-mode 1)
  (general-define-key
    :keymaps 'minibuffer-local-map
    "M-a" #'marginalia-cycle))

(use-package consult
  :config
  (my-leader-def
    "b" #'consult-buffer
    "O" #'consult-locate
    "l" #'consult-imenu
    "L" #'consult-imenu-multi)
  (general-nmmap
    "/" #'consult-line
    "gn" (defun consult-line-symbol-at-point ()
	   (interactive)
	   (consult-line (thing-at-point 'symbol)))))

(use-package flyspell-correct
  :general
  (general-nmap
    "z=" #'flyspell-correct-wrapper))

(provide 'vertico-setup)
;;; vertico-setup.el ends here
