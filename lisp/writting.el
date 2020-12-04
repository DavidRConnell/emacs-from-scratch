;;   spell-checker. Should work with magit.
;;   grammar?
;;   org
;;   org-roam
;;   org-ref
;;   org-journal?
;;   org-drill?
;;   babel
;;   ox
;;   ox-hugo
;;   ox-word
;;   Set up new notes (org?) directory with zettle and reference subdirs.
;;   Use referenece subdir for reference notes not actuall papers.
;;   Add elfeed org
;;   Deft
;;; Code:

(use-package org
  :config
  (setq org-startup-folded t
	org-hide-emphasis-markers t)

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode)
    :config
    (setq org-superstar-leading-bullet ?\s
	  org-superstar-leading-fallback ?\s
	  org-superstar-hide-leading-stars nil
	  org-superstar-todo-bullet-alist
	  '(("TODO" . 9744)
	    ("[ ]" . 9744)
	    ("DONE" . 9745)
	    ("[X]" . 9745))))

  (use-package evil-org
    :hook (org-mode . evil-org-mode)
    :config
    (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme)
    (add-hook 'org-mode-hook #'turn-on-visual-line-mode))

  (use-package org-roam
    :commands org-roam-find-file
    :init (setq org-roam-directory my-zettle-dir
                org-roam-db-location (expand-file-name "org-roam-db"
                                                       my-cache-dir))
    :hook (org-mode . org-roam-mode)
    :config
    (my-leader-def
     :infix z
     "g" #'org-roam-find-file
     "x" #'org-roam-capture)

    (add-hook 'find-file-hook
	      (lambda ()
		(if (and
		     (memq 'org-roam-buffer--update-maybe
			   post-command-hook)
		     (not (eq 'visible (org-roam-buffer--visibility))))
		    (with-current-buffer (window-buffer)
		      (org-roam-buffer--get-create)))))

    (setq org-roam-graph-viewer "/usr/bin/qutebrowser"

	  org-roam-capture-templates
	  '(("d" "default" plain #'org-roam-capture--get-point
	     :file-name "%<%Y%m%d%H%M%S>"
	     :head "#+TITLE: ${title}\n"
	     :unnarrowed t))

	  org-roam-capture-immediate-template
	  '("d" "default" plain #'org-roam-capture--get-point
	    :file-name "%<%Y%m%d%H%M%S>"
	    :head "#+TITLE: ${title}\n"
	    :unnarrowed t
	    :immediate-finish t))

    (use-package org-roam-protocol
      :straight nil
      :after org-protocol
      :config (setq org-roam-capture-ref-templates
		    '(("r" "ref" plain #'org-roam-capture--get-point
		       :file-name "%<%Y%m%d%H%M%S>"
		       :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
		       :unnarrowed t))))

    (use-package org-roam-bibtex
      :hook (org-roam-mode . org-roam-bibtex-mode)
      :config
      (setq orb-templates
	    '(("r" "ref" plain #'org-roam-capture--get-point
	       ""
	       :file-name "references/${citekey}"
	       :head "#+TITLE: ${title}\n#+AUTHOR: ${author}\n#+ROAM_KEY: ${ref}\n"
	       :unnarrowed t))
	    orb-insert-link-description 'citation))

    (use-package org-roam-server
      :config
      (setq org-roam-server-host "127.0.0.1"
	    org-roam-server-port 8080
	    org-roam-server-authenticate nil
	    org-roam-server-export-inline-images t
	    org-roam-server-serve-files nil
	    org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
	    org-roam-server-network-poll t
	    org-roam-server-network-arrows nil
	    org-roam-server-network-label-truncate t
	    org-roam-server-network-label-truncate-length 60
	    org-roam-server-network-label-wrap-length 20))))

  (use-package org-ref
    :config
    (setq org-ref-default-bibliography my-refs-bib
	  org-ref-default-ref-type "cref"
	  org-ref-bibliography-notes my-refs-notes-dir
	  org-ref-notes-function (lambda (key)
				   (interactive)
				   (org-ref-notes-function-many-files
				    key))
	  org-ref-pdf-directory my-refs-pdfs-dir
	  org-ref-get-pdf-filename-function
	  (lambda (key)
	    (let ((files (directory-files-recursively
			  org-ref-pdf-directory
			  (concat key ".pdf"))))
	      (if (= 1 (length files))
		  (car files)
		(completing-read "Choose: " files))))))

  (use-package bibtex
    :config
    (require 'find-lisp)
    (setq bibtex-completion-bibliography my-refs-bib)
    (setq bibtex-completion-additional-search-fields '(keywords))
    (setq bibtex-completion-library-path
	  (cl-remove-if-not
	   (lambda (f) (find-lisp-file-predicate-is-directory
			f
			my-refs-pdfs-dir))
	   (directory-files-recursively my-refs-pdfs-dir "." 'dirs)))
    (setq bibtex-completion-notes-path my-refs-notes-dir)
    (setq bibtex-completion-pdf-open-function
	  (lambda (fpath) (call-process "xdg-open" nil 0 nil fpath)))
    (setq bibtex-completion-notes-template-multiple-files
	  "${title}\n#+AUTHOR: ${author-or-editor}\ncite:${=key=}"))

(provide 'writting)
;;; writting ends here
