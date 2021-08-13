;;; Notes --- For note-taking -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-roam
    :init (setq org-roam-directory my-zettle-dir
		org-roam-db-location (expand-file-name "org-roam.db"
						       my-cache-dir)
		org-roam-v2-ack t)
    :general
    (my-leader-def
      :infix "n"
      "f" #'org-roam-node-find
      "x" #'org-roam-capture)
    (my-local-leader-def
      :keymaps 'org-mode-map
      :infix "m"
      "x" #'org-roam-capture
      "b" #'org-roam-switch-to-buffer
      "f" #'org-roam-node-find
      "i" #'org-roam-node-insert
      "m" #'org-roam-buffer-toggle)

    :config
    (org-roam-setup)
    (add-to-list 'display-buffer-alist
		 '("\\*org-roam\\*"
		   (display-buffer-in-direction)
		   (direction . right)
		   (window-width . 0.33)
		   (window-height . fit-window-to-buffer)))
    ;; (add-hook 'find-file-hook
    ;; 	      (lambda ()
    ;; 		(if (and
    ;; 		     (memq 'org-roam-buffer--update-maybe
    ;; 			   post-command-hook)
    ;; 		     (not (eq 'visible (org-roam-buffer--visibility))))
    ;; 		    (with-current-buffer (window-buffer)
    ;; 		      (org-roam-buffer--get-create)))))

    ;; (defun my-org-roam-find-file-action (x)
    ;;   "From https://github.com/abo-abo/oremacs/blob/15e6a33d314121ea0b3f1659dbc3ee8181dce854/modes/ora-org-roam.el"
    ;;   (if (consp x)
    ;; 	  (let ((file-path (plist-get (cdr x) :path)))
    ;; 	    (org-roam-find-file file-path))
    ;; 	(let* ((title-with-tags x)
    ;; 	       (org-roam-capture--info
    ;; 		`((title . ,title-with-tags)
    ;; 		  (slug . ,(funcall org-roam--title-to-slug-function title-with-tags))))
    ;; 	       (org-roam-capture--context 'title))
    ;; 	  (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
    ;; 	  (org-roam-capture--capture))))

    ;; (defun my-org-roam-find-file ()
    ;;   (interactive)
    ;;   (unless org-roam-mode (org-roam-mode))
    ;;   (ivy-read "File: " (org-roam--get-titles)
    ;; 		:action #'my-org-roam-find-file-action
    ;; 		:caller 'my-org-roam-find-file))

    ;; (ivy-add-actions #'my-org-roam-find-file
    ;; 		     '(("j"
    ;; 			(lambda (x) (find-file-other-window (third x)))
    ;; 			"other window")
    ;; 		       ("k"
    ;; 			(lambda (x) (delete-file (third x)))
    ;; 			"delete")
    ;; 		       ("c" (lambda (x) (kill-new (third x))) "copy file name")))
					; look into org-id-goto
    ;; (ivy-add-actions #'org-roam-switch-to-buffer
    ;; 		     '(("j"
    ;; 			(lambda (x)
    ;; 			  (switch-to-buffer-other-window (cdr x)))
    ;; 			"other window")
    ;; 		       ("k"
    ;; 			(lambda (x)
    ;; 			  (let (file-name (buffer-file-name (cdr x)))
    ;; 			    (kill-buffer file-name)
    ;; 			    (delete-file file-name)))
    ;; 			"delete")
    ;; 		       ("c" (lambda (x) (kill-new (buffer-file-name (cdr x)))) "copy file name")))

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

	  org-roam-mode-section-functions (list #'org-roam-backlinks-section
						#'org-roam-reflinks-section
						#'org-roam-unlinked-references-section))

    (add-hook 'org-roam-mode-hook #'visual-line-mode)

    (use-package org-roam-protocol
      :config
      (use-package org-protocol)
      (setq org-roam-capture-ref-templates
	    '(("r" "ref" plain ""
	       :if-new (file+head "%<%Y%m%d%H%M%S>.org"
				  "#+TITLE: ${title}\n ${ref}\n\n- tags :: %?")
	       :unnarrowed t))))

    (use-package org-roam-bibtex
      :hook (org-roam-mode . org-roam-bibtex-mode)
      :commands orb-edit-notes
      :general
      (my-local-leader-def
	:keymaps 'org-mode-map
	:infix "m"
	"r" #'orb-insert-link
	"n" #'orb-note-actions)
      :config
      (setq orb-insert-link-description 'citation))

    (use-package org-roam-ui
      :straight (:host github :repo "org-roam/org-roam-ui")
      :general
      (my-local-leader-def
	:keymaps 'org-mode-map
	:infix "m"
	"u" #'org-raom-ui-mode)
      :config
      (require 'websocket)
      (use-package simple-httpd
	:straight (simple-httpd :host github :repo "skeeto/emacs-web-server"))))

(provide 'notes)
;;; notes.el ends here
