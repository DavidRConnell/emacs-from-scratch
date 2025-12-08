;;; ui --- Improve emacs user-interface. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(save-place-mode 1)
(recentf-mode 1)
(use-package savehist
  :init
  (setq savehist-file (expand-file-name "savehist.el" my-cache-dir))
  (savehist-mode t))

(use-package which-key
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode))

(use-package evil-easymotion
  :general
  (general-define-key
   :states '(operator)
   :keymaps 'override
   "j" #'evilem-motion-next-line
   "k" #'evilem-motion-previous-line
   "C-w" #'evilem-motion-forward-word-begin
   "C-e" #'evilem-motion-forward-word-end
   "C-b" #'evilem-motion-backward-word-begin)
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
  (setq avy-keys-alist '((avy-goto-char . (?a ?o ?e ?u ?h ?t ?n ?s)))
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

  (defun my-avy-action-define (pt)
    (save-excursion
      (goto-char pt)
      (sdcv-search (thing-at-point 'word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun my-avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun my-avy-action-embark (pt)
    (unwind-protect
	(save-excursion
	  (goto-char pt)
	  (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setq avy-dispatch-alist
	'((?c . my-avy-action-kill-move)
	  (?w . my-avy-action-define)
	  (?H . my-avy-action-helpful)
	  (?i . my-avy-action-embark)
	  (?d . avy-action-kill-stay)
	  (?g . avy-action-teleport)
	  (?m . avy-action-mark)
	  (?n . avy-action-copy)
	  (?y . avy-action-yank)
	  (?k . avy-action-ispell)
	  (?z . avy-action-zap-to-char))))

(use-package evil-exchange
  :general (general-nmap "gx" #'evil-exchange))

(use-package ace-window
  :general (general-nmmap :prefix "C-w"
	     "C-w" #'ace-window
	     "C-c" #'ace-delete-window)
  :config (setq aw-keys '(?u ?h ?e ?t ?o ?n ?a ?s)
		aw-scope 'global))

(use-package popper
  :demand
  :config
  (my-leader-def
    :infix "u"
    "u" #'popper-toggle
    "n" #'popper-cycle
    "q" #'popper-kill-latest-popup
    "t" #'popper-toggle-type)

  (defun my-popper-shell-output-empty-p (buf)
    (and (string-match-p
	  "\\*Async Shell Command\\*"
	  (buffer-name buf))
	 (= (buffer-size buf) 0)))

  (setq popper-reference-buffers
	'(helpful-mode
	  "\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  (my-popper-shell-output-empty-p . hide)
	  "\\*wiki-summary\\*.*"
	  "\\*Embark Actions\\*"
	  "\\*Backtrace\\*"
	  "\\*git-gutter:diff\\*"
	  "\\*MATLAB\\*"
	  "\\*R:.*\\*"
	  "\\*Help\\*"
	  "\\*sdcv\\*"
	  "\\*lispy-message\\*"
	  "\\*Org PDF LaTeX Output\\*"
	  "\\*pytest\\*.*"
	  "\\*Python\\*"
	  "\\*python\\*"
	  "\\*eldoc.*\\*"
	  "\\*readable.*\\*"
	  Man-mode
	  compilation-mode))

  (setq popper-mode-line nil
	popper-group-function 'popper-group-by-directory)

  (require 'popper-echo)
  (popper-mode t)
  (popper-echo-mode t))

(use-package evil-args
  :general
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "a" #'evil-inner-arg)
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "a" #'evil-outer-arg))

(use-package evil-textobj-tree-sitter
  :general
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

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
    "C-r" #'undo-fu-only-redo
    "U" #'undo-fu-disable-checkpoint)
  :config
  (use-package undo-fu-session
    :config
    (global-undo-fu-session-mode t))
  (setq undo-fu-allow-undo-in-region t
	undo-fu-ignore-keyboard-quit t))

(use-package vundo
  :general
  (general-nvmap
    "U" #'vundo)
  :config
  (general-def
    :keymaps 'vundo-mode-map
    "L" #'vundo-stem-end
    "H" #'vundo-stem-root
    "j" #'vundo-next
    "k" #'vundo-previous
    [ret] #'vundo-confirm)
  (setq vundo-glyph-alist vundo-unicode-symbols))

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
        `(("." . ,(concat my-var-dir "undo-tree-hist/")))))

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
  :commands dumb-jump-xref-activate
  :general
  (general-nmmap
    :prefix "g"
    "D" #'xref-find-definitions)
  :config
  (setq dumb-jump-mode t))

(use-package xref
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

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

(use-package dirvish
  :straight t
  :config
  (general-nmap
    :keymaps 'dirvish-mode-map
    "/" #'dirvish-narrow
    "C-i" #'dirvish-history-go-forward
    "C-o" #'dirvish-history-go-backward
    "h" #'dired-up-directory
    "l" #'dired-find-file)
  (dirvish-override-dired-mode t))

(use-package vlf
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))

(use-package delim-col
  :config
  (setq delimit-columns-str-separator " | "
	delimit-columns-format 'padding))

(provide 'ui)
