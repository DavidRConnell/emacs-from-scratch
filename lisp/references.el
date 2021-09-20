;;; References --- For working with references -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See https://github.com/jkitchin/org-ref-cite &
;; https://blog.tecosaur.com/tmio/2021-07-31-citations.html for updating
;; to org-cite when released in org-mode.
(use-package org-ref
  :general
  (general-imap
    :keymaps 'org-mode-map
    "C-]" #'org-ref-insert-link)
  (my-local-leader-def
    :keymaps 'org-mode-map
    :infix "e"
    "c" #'org-ref-insert-link
    "r" #'org-ref-insert-ref-link
    "b" #'org-ref-insert-bibliography-link
    "s" #'org-ref-insert-bibliographystyle-link
    "n" #'org-ref-open-notes-at-point
    "p" #'org-ref-open-pdf-at-point
    "u" #'org-ref-open-url-at-point)
  (my-leader-def
    :infix "r"
    "c" #'org-ref-insert-link
    "d" #'doi-utils-add-bibtex-entry-from-doi
    "a" #'arxiv-get-pdf-add-bibtex-entry
    "m" (defun my-open-master-bib
	    () (interactive) (find-file my-refs-bib)))

  :config
  (setq org-ref-default-bibliography (list my-refs-bib)
	org-ref-default-ref-type "cref"
	org-ref-show-broken-links t
	org-latex-prefer-user-labels t
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

(use-package bibtex-completion
  :after org-ref
  :config
  (require 'find-lisp)
  (setq bibtex-completion-bibliography (list my-refs-bib)
	bibtex-completion-additional-search-fields '(doi url)
	bibtex-completion-library-path my-refs-pdfs-dir
	bibtex-completion-notes-path my-refs-notes-dir
	bibtex-completion-pdf-open-function
	(lambda (fpath) (call-process "xdg-open" nil 0 nil fpath))
	bibtex-completion-edit-notes-function #'orb-edit-notes)

  (general-nmap
    :keymaps 'bibtex-mode-map
    :prefix "C-c"
    "C-c" #'org-ref-clean-bibtex-entry)

  (use-package consult-bibtex
    :after embark
    :load-path "~/.cache/emacs/consult-bibtex"
    :general
    (my-leader-def
      :infix "r"
      "c" #'consult-bibtex)
    :config
    (general-def
      :keymaps 'consult-bibtex-embark-map
      "p" #'consult-bibtex-open-pdf
      "n" #'consult-bibtex-edit-notes
      "o" nil
      "e" nil)
    (general-imap
      :keymaps 'org-mode-map
      "C-]" #'consult-bibtex)
    (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map)))

  (use-package bibtex-actions
    :disabled
    :after embark
    :general
    (my-leader-def
      :infix "r"
      "c" #'bibtex-actions-insert-citation)
    :config
    ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
    (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
    (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map))
    (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))
    (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)))

(use-package ebib
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
