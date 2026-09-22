;;; ui --- Improve Emacs user-interface. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Modify the basic user interface elements of Emacs.

;;; Code:

(require 'my-variables)
(require 'my-keybindings)

(setq savehist-file (expand-file-name "savehist.el" my-var-dir))

(require 'savehist)
(require 'winner)
(require 'flymake)

(savehist-mode)
(recentf-mode)
(winner-mode)

(fset 'yes-or-no-p #'y-or-n-p)

(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-echo-area-message t)
(customize-set-variable 'initial-scratch-message nil)

(customize-set-variable 'initial-buffer-choice
			(expand-file-name "todo.org" my-zettle-dir))

(customize-set-variable 'enable-recursive-minibuffers t)

(customize-set-variable 'read-extended-command-predicate
			#'command-completion-default-include-p)

(customize-set-variable 'minibuffer-prompt-properties
			'(read-only t cursor-intangible t face minibuffer-prompt))


(setq flymake-fringe-indicator-position 'right-fringe)

(require 'fringe)

(with-eval-after-load 'diff-hl
  (customize-set-variable 'flymake-error-bitmap
			  '(diff-hl-bmp-insert modus-themes-prominent-error))
  (customize-set-variable 'flymake-warning-bitmap
			  '(diff-hl-bmp-insert modus-themes-prominent-warning))
  (customize-set-variable 'flymake-note-bitmap
			  '(diff-hl-bmp-insert modus-themes-prominent-note)))
(setq-default fringes-outside-margins t)

(setq aw-keys '(?u ?h ?e ?t ?o ?n ?a ?s)
      aw-scope 'global)

(autoload 'ace-window "ace-window")
(autoload 'ace-delete-window "ace-window")

(general-nmmap :prefix "C-w"
  "C-w" 'ace-window
  "C-c" 'ace-delete-window
  "u" 'winner-undo
  "C-r" 'winner-redo)

(autoload 'format-all-buffer "format-all")
(autoload 'format-all-mode "format-all")

(my-leader-def
  "f" 'format-all-buffer)

(with-eval-after-load 'format-all
  (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))

(straight-use-package '(aggressive-indent-mode
			:type git
			:host github
			:repo "Malabarba/aggressive-indent-mode"))
(autoload 'aggressive-indent-mode "aggressive-indent")

(setq popper-reference-buffers
      '(helpful-mode
	"\\*Messages\\*"
	"Output\\*$"
	"\\*Async Shell Command\\*"
	(my-popper-shell-output-empty-p . hide)
	"\\*Backtrace\\*"
	"\\*Help\\*"
	"\\*Org PDF LaTeX Output\\*"
	"\\*eldoc.*\\*"
	"\\*readable.*\\*"))

(setq popper-window-height #'my-popper-fit-window-height
      popper-mode-line nil
      popper-tab-line-mode nil
      popper-group-function 'popper-group-by-directory)

(require 'popper)
(require 'popper-echo)

(defvar my-popper-map (make-sparse-keymap))

(my-leader-def
  "u" '(:keymap my-popper-map :which-key "pop-ups"))

(general-def
  :keymaps 'my-popper-map
  "u" 'popper-toggle
  "n" 'popper-cycle
  "p" 'popper-cycle-backwards
  "q" 'popper-kill-latest-popup
  "t" 'popper-toggle-type)

(defun my-popper-shell-output-empty-p (buf)
  "Test if the BUF is an empty shell result buffer."
  (and (string-match-p "\\*Async Shell Command\\*" (buffer-name buf))
       (= (buffer-size buf) 0)))

(defun my-popper-add-reference (reference)
  "Add REFERENCE to `popper-reference-buffers'."
  (if (featurep 'popper)
      (add-to-list 'popper-reference-buffers reference))
  (when popper-mode (popper-mode 1)))

(defun my-popper-fit-window-height (win)
  (fit-window-to-buffer win
			(floor (frame-height) 2)
			(floor (frame-height) 3)))

(popper-mode)
(popper-echo-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(if (< emacs-major-version 28)
    (progn
      (require 'undo-fu)

      (general-nvmap
	"u" 'undo-fu-only-undo
	"C-r" 'undo-fu-only-redo
	"U" 'undo-fu-disable-checkpoint)

      (customize-set-variable 'evil-undo-system 'undo-fu)
      (customize-set-variable 'undo-fu-allow-undo-in-region t)
      (customize-set-variable 'undo-fu-ignore-keyboard-quit t)))

;; Since undo-fu is a wrapper around builtin features, saving it's history is
;; actually saving the builtin undo history and should work independently of
;; undo-fu.
(require 'undo-fu-session)
(undo-fu-session-global-mode)

(autoload 'vundo "vundo")
(general-nvmap
  "U" 'vundo)

(with-eval-after-load 'vundo
  (general-def
    :keymaps 'vundo-mode-map
    "L" 'vundo-stem-end
    "H" 'vundo-stem-root
    "j" 'vundo-next
    "k" 'vundo-previous
    [ret] 'vundo-confirm)

  (customize-set-variable 'vundo-glyph-alist vundo-unicode-symbols))

(autoload 'link-hint-open-link "link-hint")
(autoload 'link-hint-copy-link "link-hint")

(general-nmmap
  "C-/" 'link-hint-open-link
  "M-/" 'link-hint-copy-link)

(setq highlight-indent-guides-method 'character)

(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

(setq xref-show-definitions-function
      #'xref-show-definitions-completing-read)

(autoload 'xref-find-definitions "xref")

(general-nmap
  :prefix "g"
  "D" 'xref-find-definitions)

(with-eval-after-load 'xref
  (require 'dumb-jump)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(autoload 'iedit-mode "iedit")
(general-nmap
  "C-;" 'iedit-mode)

(setq dired-dwim-target t)
(with-eval-after-load 'dired
  (autoload 'dired-narrow "dired-narrow")
  (general-nmap
    :keymaps 'dired-mode-map
    "/" 'dired-narrow))

(setq delimit-columns-str-separator " | "
      delimit-columns-format 'padding)
(require 'delim-col)

(dolist (fn '(evilem-motion-next-line
	      evilem-motion-previous-line
	      evilem-motion-forward-word-begin
	      evilem-motion-forward-word-end
	      evilem-motion-backward-word-end))
  (autoload fn "evil-easymotion"))

(general-define-key
 :states '(operator)
 :keymaps 'override
 "j" 'evilem-motion-next-line
 "k" 'evilem-motion-previous-line
 "C-w" 'evilem-motion-forward-word-begin
 "C-e" 'evilem-motion-forward-word-end
 "C-b" 'evilem-motion-backward-word-begin)

(general-define-key
 :keymaps '(normal motion visual)
 "C-j" 'evilem-motion-next-line
 "C-k" 'evilem-motion-previous-line)

(with-eval-after-load 'evilem
  (evilem-make-motion evilem-motion-search-next
		      #'evil-ex-search-next
		      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-previous
		      #'evil-ex-search-previous
		      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-word-forward
		      #'evil-ex-search-word-forward
		      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-word-backward
		      #'evil-ex-search-word-backward
		      :bind ((evil-ex-search-highlight-all nil))))

(autoload 'evil-exchange "evil-exchange")
(general-nmap
  "gx" 'evil-exchange)

(autoload 'evil-inner-arg "evil-args")
(autoload 'evil-outer-arg "evil-args")

(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "a" 'evil-inner-arg)

(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "a" 'evil-outer-arg)

(with-eval-after-load 'treesit
  (autoload 'evil-textobj-tree-sitter-get-textobj "evil-textobj-tree-sitter")

  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

(dolist (fn '(evil-avy-goto-char-timer
	      evil-my-avy-goto-char-forward-in-line
	      evil-my-avy-goto-char-backward-in-line))
  (autoload fn "avy"))

(general-def
  :keymaps '(normal visual motion operator)
  "C-s" 'evil-avy-goto-char-timer
  "f" 'evil-my-avy-goto-char-forward-in-line
  "F" 'evil-my-avy-goto-char-backward-in-line
  "t" (lambda () (interactive)
	(evil-my-avy-goto-char-forward-in-line))
  "T" (lambda () (interactive)
	(evil-my-avy-goto-char-backward-in-line)))

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
	(?z . avy-action-zap-to-char)))

(setq avy-keys-alist '((avy-goto-char . (?a ?o ?e ?u ?h ?t ?n ?s)))
      avy-keys '(?u ?h ?e ?t ?o ?n ?a ?s)
      avy-enter-times-out t
      avy-timeout-seconds 0.3
      avy-flyspell-correct-function #'flyspell-correct-at-point)

(with-eval-after-load 'avy
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

  )

(setq evil-goggles-duration 0.1
      evil-goggles-pulse t
      evil-goggles-enable-change nil
      evil-goggles-enable-delete nil)

(require 'evil-goggles)
(require 'evil-surround)

(evil-goggles-mode)

(global-evil-surround-mode)

(autoload 'evil-lion-left "evil-lion")
(autoload 'evil-lion-right "evil-lion")

(general-nvmap
  :prefix "g"
  "l" 'evil-lion-left
  "L" 'evil-lion-right)

(autoload 'evilnc-comment-or-uncomment-lines "evil-nerd-commenter")
(autoload 'evilnc-comment-operator "evil-nerd-commenter")

(general-nmap
  :prefix "g"
  "c" (general-key-dispatch
	  'evilnc-comment-operator
	"c" 'evilnc-comment-or-uncomment-lines))

(setq hl-todo-highlight-punctuation ":")
(setq hl-todo-keyword-faces
      '(("TODO"       nano-face-header-default bold)
	("FIXME"      next-error bold)
	("HACK"       font-lock-constant-face bold)
	("REVIEW"     hydra-face-red bold)
	("NOTE"       success bold)
	("WARNING"    hydra-face-red bold)
	("DEPRECATED" font-lock-doc-face bold)
	("TEMP"       modus-themes-prominent-warning bold)))

(setq my-todo-keywords (mapcar (lambda (x) (car x)) hl-todo-keyword-faces))

(require 'hl-todo)
(with-eval-after-load 'flymake
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake))

(global-hl-todo-mode)

(provide 'my-ui)
;;; my-ui.el ends here
