;;; Notes --- For note-taking -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-roam
  :init (setq org-roam-directory my-zettle-dir
	      org-roam-db-location (expand-file-name "org-roam.db"
						     my-var-dir)
	      org-roam-v2-ack t)
  :defer 3
  :general
  (my-leader-def
    :infix "n"
    "f" #'org-roam-node-find
    "x" #'org-roam-capture
    "r" #'org-roam-node-random
    "d" #'org-roam-dailies-goto-today
    "D" #'org-roam-dailies-goto-date)
  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "m"
    "x" #'org-roam-capture
    "f" #'org-roam-node-find
    "i" #'org-roam-node-insert
    "m" #'org-roam-buffer-toggle
    "M" #'org-roam-buffer-display-dedicated
    "c" #'org-id-get-create)
  :config
  (org-roam-setup)
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . frame-height)))

  (general-def
    ;; Used by org-roam-buffer
    :keymaps 'magit-section-mode-map
    "C-j" #'magit-section-forward
    "C-k" #'magit-section-backward
    "C-i" #'magit-section-cycle)
  (general-def
    :keymaps 'org-roam-node-map
    "C-c C-o" (lambda ()
		(interactive)
		(org-roam-node-visit
		 (org-roam-node-at-point t) t t)))
  (general-def
    :keymaps 'org-roam-preview-map
    "C-c C-o" (lambda ()
		(interactive)
		(org-roam-preview-visit
		 (org-roam-buffer-file-at-point 'assert)
		 (oref (magit-current-section) point)
		 t)))

  (require 'org-roam-dailies)
  (setq org-roam-completion-everywhere t
	org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>.org"
			      "#+TITLE: ${title}\n\n- tags :: ")
	   :jump-to-captured t
	   :unnarrowed t)
	  ("r" "reference" plain "%?"
	   :if-new (file+head "references/${citekey}.org"
			      "#+TITLE: ${title}\n#+AUTHOR: ${author}\n\n- tags :: [[roam:Read]]")
	   :jump-to-captured t
	   :unnarrowed t)
	  ("i" "immediate" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>.org"
			      "#+TITLE: ${title}\n")
	   :unnarrowed t
	   :immediate-finish t))

	org-roam-mode-section-functions
	(list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section)
	org-roam-dailies-directory "dailies/"
	org-roam-dailies-capture-templates
	'(("d" "default" entry "* %?"
	   :target (file+head "%<%Y%m%d>.org"
			      "#+TITLE: %<%Y-%m-%d>\n"))))

  (add-hook 'org-roam-mode-hook #'visual-line-mode))

(use-package org-roam-protocol
  :after org-roam
  :config
  (use-package org-protocol)
  (setq org-roam-capture-ref-templates
	'(("b" "bookmark" plain "%?"
	   :if-new (file+head "bookmarks/%<%Y%m%d%H%M%S>.org"
			      "#+TITLE: ${title}\n ${ref}\n\n- tags :: %?")
	   :unnarrowed t
	   :immediate-finish t))))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "m"
    "r" #'orb-insert-link
    "n" #'orb-note-actions)
  (setq orb-insert-link-description 'citation))

(use-package org-roam-ui
  :after org-roam
  :general
  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "m"
    "u" #'org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(provide 'notes)
;;; notes.el ends here
