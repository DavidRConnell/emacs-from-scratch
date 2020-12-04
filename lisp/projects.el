;;   projectile
;;   persp?
;;   Does this need to be separate from workspaces?
;;   grep project tool

(use-package projectile
  :general
  (my-leader-def
    :infix "p"
    "o" #'projectile-switch-project)
  (my-leader-def
    "," #'projectile-run-vterm
    "." (lambda () (interactive)
	  (projectile-find-file-in-directory user-emacs-directory)))
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'default
	projectile-cache-file (expand-file-name "projects/" my-cache-dir))
  :config
  (use-package ripgrep
    :general
    (my-leader-def
      :infix "p"
      "g" #'projectile-ripgrep)))

(provide 'projects)
;;; projects.el ends here
