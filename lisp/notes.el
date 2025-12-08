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
    "c" #'org-id-get-create
    "aa" #'org-roam-alias-add
    "ar" #'org-roam-ref-add)
  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "d"
    "d" #'org-roam-dailies-goto-today
    "n" #'org-roam-dailies-goto-next-note
    "p" #'org-roam-dailies-goto-previous-note)
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
	   :target (file+head "%<%Y%m%d%H%M%S>.org"
			      "#+TITLE: ${title}\n\n- tags :: ")
	   :jump-to-captured t
	   :unnarrowed t)
	  ("r" "reference" plain "%?"
	   :target (file+head "references/${citar-citekey}.org"
			      "#+TITLE: ${citar-title}\n#+AUTHOR: ${citar-author}\n#+YEAR: ${citar-date}\n")
	   :jump-to-captured t
	   :unnarrowed t)
	  ("i" "immediate" plain "%?"
	   :target (file+head "%<%Y%m%d%H%M%S>.org"
			      "#+TITLE: ${title}\n")
	   :unnarrowed t
	   :immediate-finish t)
	  ("f" "fleeting" entry "* ${title}%?"
	   :target (node "Inbox")
	   :unnarrowed t))

	org-roam-mode-sections
	(list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section)
	org-roam-dailies-directory "dailies/"
	org-roam-dailies-capture-templates
	'(("d" "default" entry "* %?"
	   :target (file+head "%<%Y%m%d>.org"
			      "#+TITLE: %<%Y-%m-%d>\n"))))

  (add-hook 'org-roam-mode-hook #'visual-line-mode)
  (add-hook 'org-after-todo-state-change-hook
	    #'(lambda ()
		(if (org-entry-is-todo-p)
		    (org-reset-checkbox-state-subtree)))))

(use-package org-roam-protocol
  :after org-roam
  :config
  (require 'org-protocol)
  (setq org-roam-capture-ref-templates
	'(("b" "bookmark" plain "%?"
	   :target (file+head "bookmarks/${slug}.org"
			      "#+TITLE: ${title}\n ${ref}\n\n- tags :: ")
	   :unnarrowed t))))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq orb-insert-link-description 'citation-org-cite
	orb-roam-ref-format 'org-cite
	org-roam-mode-sections
	(list #'org-roam-backlinks-section
	      #'orb-section-abstract
	      #'org-roam-reflinks-section)))

(provide 'notes)
;;; notes.el ends here
