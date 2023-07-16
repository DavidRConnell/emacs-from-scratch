;;; References --- For working with references -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-ref
  :defer 3
  :general
  (my-leader-def
    :infix "r"
    "d" #'doi-utils-add-bibtex-entry-from-doi
    "a" #'arxiv-get-pdf-add-bibtex-entry
    "i" #'isbn-to-bibtex
    "m" (defun my-open-master-bib
	    () (interactive) (find-file my-refs-bib)))

  :config
  (require 'org-ref-arxiv)
  (require 'org-ref-isbn)
  (add-to-list 'org-ref-bibtex-journal-abbreviations
	       '("CC" "Cerebral Cortex" "Cereb. Cortex")))

(use-package citeproc)

(use-package bibtex
  :after org-ref
  :mode ("\\.bib\\'" . bibtex-mode)
  :config
  (general-nmap
    :keymaps 'bibtex-mode-map
    :prefix "C-c"
    "C-c" #'org-ref-clean-bibtex-entry)

  ;; (add-hook 'bibtex-add-entry-hook #'citar-refresh)
  )

(use-package oc
  :demand
  :config
  (require 'oc-csl)
  (setq org-cite-csl-styles-dir "~/.local/share/csl"
	org-cite-export-processors
	;; Potentially modify to use natbib for latex (see variable's
	;; help).
	'((t csl)))
  (set-face-attribute 'org-cite nil :foreground nano-color-faded)
  (set-face-attribute 'org-cite-key nil :foreground nano-color-faded))

(use-package citar
  :demand
  :after embark
  ;; :hook
  ;; (LaTeX-mode . citar-capf-setup)
  ;; (markdown-mode . citar-capf-setup)
  ;; (org-mode . citar-capf-setup)
  :general
  (my-leader-def
    :infix "r"
    "c" #'citar-open)
  (general-imap
    :keymaps 'org-mode-map
    "C-]" (lambda ()
	    (interactive)
	    (let* ((bib (citar-org-local-bib-files))
		   (citar-bibliography (if bib
					   bib citar-bibliography)))
	      (if bib
		  (progn
		    (setq-local citar--candidates-cache 'uninitialized
				citar--local-candidates-cache 'uninitialized)))
	      (call-interactively #'org-cite-insert))))

  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "m"
    "n" #'embark-act)
  :config
  (require 'citar-capf)
  (require 'citar-org)
  (require 'citar-citeproc)
  (setq org-cite-global-bibliography (list my-refs-bib)
	org-cite-insert-processor 'citar
	org-cite-follow-processor 'citar
	org-cite-activate-processor 'citar
	citar-bibliography org-cite-global-bibliography
	citar-library-paths (list my-refs-pdfs-dir my-refs-books-dir)
	citar-file-additional-files-separator "_supp-"
	citar-notes-paths (list my-refs-notes-dir)
	citar-citeproc-csl-styles-dir org-cite-csl-styles-dir
	citar-notes-sources '((citar-file :name "Notes"
					  :category file
					  :items citar-file--get-notes
					  :hasitems citar-file--has-notes
					  :open find-file
					  :create orb-citar-edit-note
					  :transform file-name-nondirectory))
	citar-at-point-function 'embark-act
	bibtex-completion-bibliography (list my-refs-bib)
	bibtex-completion-notes-path my-refs-notes-dir
	bibtex-completion-library-path citar-library-paths)
  (add-to-list 'savehist-additional-variables 'citar-history)

  (setq citar-file-open-functions
	'(("html" . citar-file-open-external)
	  ("pdf" . citar-file-open-external)
	  (t . find-file)))
  (general-def
    :keymaps '(citar-citation-map citar-map)
    "f" nil
    "p" #'citar-open-files
    "a" (defun my-add-library-file (key)
	  "Add a file to reference library associated with KEY."
	  (interactive (list (citar-select-ref)))
	  (message "%s" key)
	  (bibtex-completion-add-pdf-to-library (list key))))

  ;; (advice-add #'citar--select-resource
  ;; 	      :filter-args
  ;; 	      (defun advise-citar--select-resource-no-prompt (arg-list)
  ;; 		"Advise around citar--select-resource to never prompt if only one resource found."
  ;; 		(let ((prompt-pos (seq-position arg-list :always-prompt)))
  ;; 		  (if prompt-pos
  ;; 		      (setq arg-list (seq-union (seq-subseq arg-list 0 prompt-pos)
  ;; 						(seq-subseq arg-list (+ prompt-pos 2))))))

  ;; 		(seq-union arg-list '(:always-prompt nil))))
  ;; (advice-remove #'advise-citar--select-resource-no-prompt #'citar-select-resource)

  (defun my-org-roam-citation-finder ()
    "Return the citation keys for the currently visited org-roam reference note."
    (when-let (property (and (eq major-mode 'org-mode)
			     (car (org-property-values "ROAM_REFS"))))
      (if (string-match org-element-citation-key-re property)
	  (let ((key (match-string 1 property)))
	    (cons 'citar-key key)))))

  (add-to-list 'embark-target-finders #'my-org-roam-citation-finder))

(use-package citar-embark
  :after citar embark
  :config (citar-embark-mode))

(use-package citar-org-roam
  :after citar org-roam
  :config
  (setq citar-org-roam-capture-template-key "r")
  (citar-org-roam-mode))

(use-package ebib
  :commands ebib
  :general
  (my-leader-def
    :infix "r"
    "e" (defun my-ebib-open-with-file ()
	  "Open with bib file in PWD if exists otherwise use master.

If there is more than one local bib file ask."
	  (interactive)
	  (let* ((potential-bibs (citar-org-local-bib-files))
		 (local-bib-file
		  (cond
		   ((not potential-bibs)
		    nil)
		   ((= (length potential-bibs) 1)
		    (cl-first potential-bibs))
		   ((> (length potential-bibs) 1)
		    (completing-read "Select bib" potential-bibs)))))
	    (if local-bib-file
		(ebib (expand-file-name local-bib-file))
	      (ebib my-refs-bib)))))
  :config
  (setq ebib-notes-directory my-refs-notes-dir
	ebib-reading-list-file (concat my-refs-notes-dir "readinglist.org")
	ebib-default-directory my-refs-notes-dir
	ebib-keywords-file (concat my-refs-notes-dir ".keywords.txt")
	ebib-notes-show-note-method nil
	ebib-file-associations '(("pdf" . "xdg-open")))

  (require 'find-lisp)
  (setq ebib-file-search-dirs
	(cl-remove-if-not
	 (lambda (f) (find-lisp-file-predicate-is-directory f my-refs-pdfs-dir))
	 (directory-files-recursively my-refs-pdfs-dir "." 'dirs)))

  (general-define-key
   :keymaps 'ebib-index-mode-map
   "/" #'swiper)

  (defun my-ebib-create-org-title (key db)
    (replace-regexp-in-string "[\t\n ]+"
			      " "
			      (or (ebib-get-field-value
				   "title" key db 'noerror 'unbraced 'xref)
				  "(No Title)")))

  (defun my-ebib-create-org-author (key db)
    (replace-regexp-in-string "[\t\n ]+ "
			      " "
			      (or (ebib-get-field-value
				   "author" key db 'noerror 'unbraced 'xref)
				  (ebib-get-field-value
				   "editor" key db 'noerror 'unbraced 'xref)
				  "(No Author)")))

  (defun my-ebib-create-org-identifier (key _)
    key)

  (setq ebib-notes-template-specifiers
	'((?K . ebib-create-org-identifier)
	  (?k . my-ebib-create-org-identifier)
	  (?T . ebib-create-org-title)
	  (?t . my-ebib-create-org-title)
	  (?A . my-ebib-create-org-author)
	  (?L . ebib-create-org-link)
	  (?F . ebib-create-org-file-link)
	  (?D . ebib-create-org-doi-link)
	  (?U . ebib-create-org-url-link)
	  (?M . ebib-reading-list-todo-marker)))

  (setq ebib-reading-list-template-specifiers ebib-notes-template-specifiers)

  (setq ebib-reading-list-template "* %M [[file:%k.org][%t]]\n:PROPERTIES:\n:AUTHOR: %A\n:END:\ncite:%k\n\n")
  (setq ebib-notes-template "#+TITLE: %t\n#+AUTHOR: %A\ncite:%k\n\n>|<"))

(use-package deft
  :after org-roam
  :general
  (my-leader-def
    :infix "n"
    "d" #'deft)
  :config
  (defun deft-parse-title (file contents)
    "HACK to work around deft's defualt method of finding a
title. Force deft to search for org-mode title so it returns a
relevant line."
    (if deft-use-filename-as-title
	(deft-base-filename file)

      (let ((begin (string-match "^#\\+TITLE:.*$" contents)))
	(if begin
	    (funcall deft-parse-title-function
		     (substring contents begin (match-end 0)))))))

  (setq deft-default-extension "org"
	deft-new-file-format "%Y%m%d%H%M%S"
	deft-use-filter-string-for-filename nil
	deft-directory org-roam-directory)

  (general-imap
    :keymaps 'deft-mode-map
    "C-o" #'deft-open-file-other-window
    "C-w" #'deft-filter-decrement-word
    "C-n" #'next-line
    "C-p" #'previous-line))

(provide 'references)
;;; references.el ends here
