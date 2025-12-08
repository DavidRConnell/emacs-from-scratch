;;; completion --- Setup completion types -*- lexical-binding: t -*-
;;   counsel/ivy
;;   company
;;   yas
;;   file-snippets or whatever it is. Let's make sure keeping good formed files.
;;; Commentary:
;;; Code:

(use-package prescient
  :custom
  (prescient-aggressive-file-save t)
  (prescient-sort-length-enable nil)
  (prescient-sort-full-matches-first t)
  (prescient-history-length 200)
  (prescient-frequency-decay 0.997)
  (prescient-frequency-threshold 0.05)
  :config
  (prescient-persist-mode +1))

(require 'vertico-setup)

(use-package tempel
  :after (corfu cape)
  :disabled t
  :commands tempel-complete
  :general
  (general-imap
    "C-e" #'tempel-expand)
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
		      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (general-def
    :map tempel-map
    "M-n" #'tempel-next
    "M-p" #'tempel-previous
    "C-RET" #'tempel-done
    "C-c C-k" #'tempel-abort))

(use-package tempel-collection
  :load-path "/home/voidee/.config/emacs/packages/tempel-collection"
  :after tempel
  :config
  (defun tempel-collection-reload ()
    (interactive)
    (setq tempel-collection--loaded nil)))

(use-package yasnippet
  :after yasnippet-capf
  :general
  (general-imap
    "C-e" #'yas-expand)
  :config
  (yas-global-mode t)
  (use-package doom-snippets
    :load-path "~/.cache/emacs/doom-snippets/"
    :config
    (setq doom-snippets-dir
	  (expand-file-name "doom-snippets/" my-cache-dir))
    (yas-reload-all))

  (general-define-key
   :keymaps 'yas-keymap
   "M-n" #'yas-next-field-or-maybe-expand
   "M-p" #'yas-prev-field)

  (add-to-list 'yas/snippet-dirs (expand-file-name "snippets"
						   user-emacs-directory))
  (yas-reload-all))

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'completion)
;;; completion.el ends here
