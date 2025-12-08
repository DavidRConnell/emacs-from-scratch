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

(use-package yasnippet
  :hook ((text-mode prog-mode snippet-mode) .
	 yas-minor-mode-on)
  :general
  (general-imap
    "C-e" #'yas-expand)
  :config
  (use-package doom-snippets
    :load-path "~/.cache/emacs/doom-snippets/"
    :config
    (setq doom-snippets-dir
	  (expand-file-name "doom-snippets/" my-cache-dir))
    (yas-reload-all))

  (general-define-key
   :keymaps 'yas-keymap
   "C-SPC" #'yas-next-field-or-maybe-expand)

  (add-to-list 'yas/snippet-dirs (expand-file-name "snippets"
						   user-emacs-directory))
  (yas-reload-all))

(provide 'completion)
;;; completion.el ends here
