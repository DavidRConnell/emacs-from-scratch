;;;; file-templates.el --- Autoinsert templates in empty files -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yatemplate
  :after yasnippet
  :straight t
  :config
  (auto-insert-mode t)
  (setq yatemplate-dir (expand-file-name "templates"
					 user-emacs-directory))
  (yatemplate-fill-alist))

(provide 'file-templates)
;;; file-tempalets.el ends here
