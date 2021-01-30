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
    "g" #'counsel-projectile-rg
    "b" #'counsel-projectile-switch-to-buffer))

(use-package eyebrowse
  :config
  (general-define-key
   :keymaps 'override
   :prefix "C-c C-w"
   "r" #'eyebrowse-rename-window-config
   "c" #'eyebrowse-create-window-config
   "b" #'eyebrowse-switch-to-window-config)
  (general-nmap
    :prefix "g"
    "t" #'eyebrowse-next-window-config
    "T" #'eyebrowse-prev-window-config)
  (general-nmap
    :prefix "z"
    "x" #'eyebrowse-close-window-config)
  (eyebrowse-mode t)
  (setq eyebrowse-wrap-around t
	eyebrowse-new-workspace t))

(use-package golden
  :straight (golden :type git :repo "https://git.sr.ht/~wklew/golden")
  :config (global-golden-mode 1))

(provide 'projects)
;;; projects.el ends here
