;;; References --- For working with references -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-ref
  :defer 3
  :general
  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "e"
    "r" #'org-ref-insert-ref-link
    "b" #'org-ref-insert-bibliography-link
    "s" #'org-ref-insert-bibliographystyle-link)

  (my-leader-def
    :infix "r"
    "d" #'doi-utils-add-bibtex-entry-from-doi
    "a" #'arxiv-get-pdf-add-bibtex-entry
    "m" (defun my-open-master-bib
	    () (interactive) (find-file my-refs-bib)))

  :config
  (require 'org-ref-arxiv)
  (setq org-ref-default-ref-type "cref"
	org-latex-prefer-user-labels t)
  (add-to-list 'org-ref-bibtex-journal-abbreviations
	       '("CC" "Cerebral Cortex" "Cereb. Cortex")))

(use-package org-ref-cite
  :disabled
  :load-path "~/.cache/emacs/org-ref-cite/"
  :general
  ;; (my-local-leader-def
  ;;   :keymaps 'org-mode-map
  ;;   :infix "e"
  ;;   "r" #'org-cite-ref-insert-ref-link
  ;;   "b" #'org-cite-ref-insert-bibliography-link
  ;;   "s" #'org-cite-ref-insert-bibliographystyle-link)

  (my-leader-def
    :infix "r"
    "d" #'doi-utils-add-bibtex-entry-from-doi
    "a" #'arxiv-get-pdf-add-bibtex-entry
    "m" (defun my-open-master-bib
	    () (interactive) (find-file my-refs-bib)))
  :config
  (setq org-cite-global-bibliography bibtex-completion-bibliography
	org-cite-insert-processor 'org-ref-cite
	org-cite-follow-processor 'org-ref-cite
	org-cite-activate-processor 'org-ref-cite))

;; (use-package citeproc
;;   :after org)

(use-package org-ref-cite-core
  :disabled
  :load-path "~/.cache/emacs/org-ref-cite"
  :after (org-ref oc bibtex-completion)
  :config
  (setq org-cite-export-processors '((html csl "elsevier-with-titles.csl")
				     (latex org-ref-cite)
				     (t basic))))

(use-package bibtex-completion
  :after (org-ref org-roam-bibtex)
  :config
  (require 'find-lisp)
  (setq bibtex-completion-bibliography (list my-refs-bib)
	bibtex-completion-additional-search-fields '(doi url)
	bibtex-completion-library-path my-refs-pdfs-dir
	bibtex-completion-notes-path my-refs-notes-dir
	bibtex-completion-edit-notes-function #'orb-bibtex-completion-edit-note
	bibtex-completion-pdf-open-function
	(lambda (fpath) (call-process "xdg-open" nil 0 nil fpath)))
  (add-to-list 'bibtex-completion-format-citation-functions
	       '(org-mode . (lambda (keys)
			      (format "cite:%s"
				      (s-join ","
					      (--map (format "%s" it) keys))))))

  (general-nmap
    :keymaps 'bibtex-mode-map
    :prefix "C-c"
    "C-c" #'org-ref-clean-bibtex-entry))

(use-package consult-bibtex
  :load-path "~/.cache/emacs/consult-bibtex"
  :after bibtex-completion
  :general
  (my-leader-def
    :infix "r"
    "c" #'consult-bibtex-insert-citation)

  (general-imap
    :keymaps 'org-mode-map
    "C-]" #'consult-bibtex-insert-citation)

  :config
  (general-define-key
   :keymaps 'consult-bibtex-embark-map
   "o" #'nil
   "e" #'nil
   "p" #'consult-bibtex-open-pdf
   "n" #'consult-bibtex-edit-notes)
  (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map)))

(use-package bibtex-actions
  :disabled
  :after (bibtex-completion embark oc org-roam-bibtex)
  :general
  :config
  (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
  (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq bibtex-actions-bibliography (list my-refs-bib)
	bibtex-actions-file-open-note-function #'orb-bibtex-actions-edit-note
	bibtex-actions-at-point-function 'embark-act))

(use-package oc-bibtex-actions
  :disabled
  :after (embark oc)
  :config
  (setq org-cite-insert-processor 'oc-bibtex-actions
	org-cite-follow-processor 'oc-bibtex-actions
	org-cite-activate-processor 'oc-bibtex-actions))

(use-package ebib
  :straight t
  :general
  (my-leader-def
    :infix "r"
    "e" (defun my-ebib-open-with-file ()
	  "Open with bib file in PWD if exists otherwise use master.

If there is more than one local bib file ask."
	  (interactive)
	  (let* ((potential-bibs (org-ref-find-bibliography))
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
