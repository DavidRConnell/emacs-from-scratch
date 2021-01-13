;;; projects --- For managing your projects -*- lexical-binding: t; -*-
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
    "SPC" #'projectile-find-file
    "," (defun my-open-term-in-project-or-dir ()
	  "If in a projectile recognized directory open term in project root.
Otherwise open in `default-directory'."

	  (interactive)
	  (if (projectile-project-p)
	      (my-term (projectile-project-root))
	    (my-term)))

    "." (defun my-find-dot-file ()
	  "Find a file in `user-emacs-directory'."
	  (interactive)
	  (projectile-find-file-in-directory user-emacs-directory)))
  :config
  (setq projectile-completion-system 'default
	projectile-cache-file (expand-file-name "projects/" my-cache-dir))

  (projectile-mode +1))

(use-package counsel-projectile
  :after (projectile ivy counsel)
  :config
  (counsel-projectile-mode 1)
  (my-leader-def
    :infix "p"
    "g" #'counsel-projectile-rg))

  :config
  (use-package ripgrep
    :general
    (my-leader-def
      :infix "p"
      "g" #'projectile-ripgrep)))

(use-package workgroups2
  :init
  (setq wg-session-file (expand-file-name "workgroups" my-var-dir))
  :general
  (my-leader-def
    :infix "p"
    "p" #'wg-switch-to-workgroup
    "w" #'wg-open-workgroup
    "c" #'wg-create-workgroup)
  :config
  (defun wg-change-modeline ())
  (workgroups-mode 1))

(use-package golden-ratio
  :disable
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (golden-ratio-mode 1))

(use-package golden
  :straight (golden :type git :repo "https://git.sr.ht/~wklew/golden")
  :config (global-golden-mode 1))

(provide 'projects)
;;; projects.el ends here
