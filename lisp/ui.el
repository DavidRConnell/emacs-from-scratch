;;; ui --- Improve emacs user-interface. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(save-place-mode 1)

(recentf-mode 1)

(use-package which-key
  :config (which-key-mode))

(use-package evil-easymotion
  :general
  (general-define-key
   :states '(operator)
   :keymaps 'override
    "j" #'evilem-motion-next-line
    "k" #'evilem-motion-previous-line)
  (general-define-key
   :keymaps '(normal motion visual)
   "C-j" #'evilem-motion-next-line
   "C-k" #'evilem-motion-previous-line))

(use-package avy
  :general (general-define-key
	    :keymaps '(normal visual motion operator)
	    "C-s" #'evil-avy-goto-char-timer
	    "f" #'evil-my-avy-goto-char-forward-in-line
	    "F" #'evil-my-avy-goto-char-backward-in-line
	    "t" #'(lambda () (interactive)
		    (evil-my-avy-goto-char-forward-in-line))
	    "T" #'(lambda () (interactive)
		    (evil-my-avy-goto-char-backward-in-line)))
  :config
  (setq avy-keys-alist '((avy-goto-char . (?u ?h ?e ?t ?o ?n ?a ?s)))
	avy-keys '(?u ?h ?e ?t ?o ?n ?a ?s)
	avy-enter-times-out t
	avy-timeout-seconds 0.3
	avy-flyspell-correct-function #'flyspell-correct-at-point)

  (defun my-avy-goto-char-forward-in-line (char)
    "Jump to the currently visible CHAR in the current line."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy-jump
       (regexp-quote (string char))
       :beg (point)
       :end (line-end-position))))

  (defun my-avy-goto-char-backward-in-line (char)
    "Jump to the currently visible CHAR in the current line."
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy-jump
       (regexp-quote (string char))
       :beg (line-beginning-position)
       :end (point))))

  (evil-define-avy-motion my-avy-goto-char-forward-in-line inclusive)
  (evil-define-avy-motion my-avy-goto-char-backward-in-line inclusive)

  (defun my-avy-action-kill-move (pt)
    "Kill sexp at PT and move there."
    (goto-char pt)
    (avy-forward-item)
    (kill-region pt (point))
    (message "Killed: %s" (current-kill 0))
    (point)
    (evil-insert-state))

  (setq avy-dispatch-alist
        '((?c . my-avy-action-kill-move)
          (?d . avy-action-kill-stay)
          (?g . avy-action-teleport)
          (?m . avy-action-mark)
          (?n . avy-action-copy)
          (?y . avy-action-yank)
          (?i . avy-action-ispell)
          (?z . avy-action-zap-to-char))))

(use-package evil-exchange
  :general (general-nmap "gx" #'evil-exchange))

(use-package ace-window
  :general (general-nmmap :prefix "C-w"
	     "C-w" #'ace-window
	     "C-c" #'ace-delete-window)
  :config (setq aw-keys '(?u ?h ?e ?t ?o ?n ?a ?s)
		aw-scope 'frame))

(use-package evil-args
  :general
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "a" #'evil-inner-arg)
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "a" #'evil-outer-arg))

(use-package evil-lion
  :general
  (general-nvmap
    :prefix "g"
    "l" #'evil-lion-left
    "L" #'evil-lion-right))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package undo-fu
  :preface (setq evil-undo-system 'undo-fu)
  :general
  (general-nvmap
    "u" #'undo-fu-only-undo
    "C-r" #'undo-fu-only-redo)
  :config
  (use-package undo-fu-session
    :config
    (global-undo-fu-session-mode t)))

(use-package undo-tree
  :disabled
  :hook ((prog-mode text-mode) . undo-tree-mode)
  :preface
  (setq evil-undo-system 'undo-tree)
  :general
  (general-nmap
    "U" #'undo-tree-visualize)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000
        undo-tree-history-directory-alist
        `(("." . ,(concat my-cache-dir "undo-tree-hist/")))))

(use-package link-hint
  :general
  (general-nmmap
    "C-/" #'link-hint-open-link
    "M-/" #'link-hint-copy-link))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package dumb-jump
  :general
  (general-nmmap
    :prefix "g"
    "D" #'xref-find-definitions)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package iedit
  :general
  (general-nmap
   "C-;" #'iedit-mode))

(use-package dired-narrow
  :general
  (general-nmap
    :keymaps 'dired-mode-map
    "/" #'dired-narrow))

(setq dired-dwim-target t)

(provide 'ui)
