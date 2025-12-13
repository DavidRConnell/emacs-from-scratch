;;; my-references.el --- Reference support in Org -*- lexical-binding: t -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Adds support for references. Primarily for working with org-mode but
;; includes features outside such as ebib and managing bib files in general.

;;; Code:

(require 'my-variables)
(require 'my-keybindings)

(autoload 'bibtex-completion-add-pdf-to-library "bibtex-completion")
(autoload 'org-ref-clean-bibtex-entry "org-ref-bibtex" nil t)

(with-eval-after-load 'bibtex-completion
  (customize-set-variable 'bibtex-completion-bibliography my-refs-master-bib)
  (customize-set-variable 'bibtex-completion-notes-path my-refs-notes-dir)
  (customize-set-variable 'bibtex-completion-library-path my-refs-stores))

(with-eval-after-load 'org-ref-bibtex
  (require 'bibtex-completion)

  (general-nmap
    :keymaps 'bibtex-mode-map
    :prefix "C-c"
    "C-c" 'org-ref-clean-bibtex-entry)

  (add-to-list 'org-ref-bibtex-journal-abbreviations
	       '("CC" "Cerebral Cortex" "Cereb. Cortex")))

(autoload 'arxiv-get-pdf-add-bibtex-entry "org-ref-arxiv" nil t)
(autoload 'doi-utils-add-bibtex-entry-from-doi "doi-utils" nil t)
(autoload 'isbn-to-bibtex "org-ref-isbn" nil t)

(general-def
  :keymaps 'my-reference-map
  "d" 'doi-utils-add-bibtex-entry-from-doi
  "a" 'arxiv-get-pdf-add-bibtex-entry
  "i" 'isbn-to-bibtex
  "m" (defun my-open-master-bib ()
	(interactive)
	(find-file my-refs-master-bib)))

(with-eval-after-load 'org
  (require 'oc)

  (customize-set-variable 'org-cite-export-processors
			  '((latex biblatex) (t csl)))

  (set-face-attribute 'org-cite nil :foreground nano-color-faded)
  (set-face-attribute 'org-cite-key nil :foreground nano-color-faded))

(with-eval-after-load 'oc-csl
  (customize-set-variable 'org-cite-csl-styles-dir
			  (expand-file-name "csl/styles"
					    (getenv "XDG_DATA_HOME"))))

(with-eval-after-load 'oc
  (require 'citar)
  (require 'citar-org)

  (general-def
    :keymaps 'my-reference-map
    "c" 'citar-open)

  (defun my-citar-insert ()
    "Use citar to insert a citation but use the local bib file if available."
    (interactive)
    (let* ((bib (citar-org-local-bib-files))
	   (citar-bibliography (if bib
				   bib citar-bibliography)))

      (call-interactively #'org-cite-insert)))

  (general-imap
    :keymaps 'org-mode-map
    "C-]" 'my-citar-insert)

  (customize-set-variable 'org-cite-global-bibliography my-refs-bibs)
  (customize-set-variable 'org-cite-insert-processor 'citar)
  (customize-set-variable 'org-cite-follow-processor 'citar)
  (customize-set-variable 'org-cite-activate-processor 'citar)

  (customize-set-variable 'citar-bibliography my-refs-bibs)
  (customize-set-variable 'citar-library-paths my-refs-stores)
  (customize-set-variable 'citar-file-additional-files-separator "_supp-")
  (customize-set-variable 'citar-notes-paths (list my-refs-notes-dir))
  (customize-set-variable 'citar-file-open-functions
			  '(("html" . citar-file-open-external)
			    ("pdf" . citar-file-open-external)
			    (t . find-file)))

  (add-to-list 'savehist-additional-variables 'citar-history)

  (general-def
    :keymaps '(citar-citation-map citar-map)
    "C-SPC" 'embark-cycle
    "x" 'citar-open-files
    "a" (defun my-add-library-file (key)
	  "Add a file to reference library associated with KEY."
	  (interactive (list (citar-select-ref)))
	  (message "%s" key)
	  (bibtex-completion-add-pdf-to-library (list key))))

  (defun my-org-roam-citation-finder ()
    "Return the citation keys for the currently visited org-roam reference note."
    (when-let (property (and (eq major-mode 'org-mode)
			     (car (org-property-values "ROAM_REFS"))))
      (if (string-match org-element-citation-key-re property)
	  (let ((key (match-string 1 property)))
	    (cons 'citar-key key)))))

  (with-eval-after-load 'embark
    (require 'citar-embark)

    (customize-set-variable 'citar-at-point-function 'embark-act)
    (add-to-list 'embark-target-finders #'my-org-roam-citation-finder)
    (citar-embark-mode)))

(autoload 'ebib "ebib")

(general-def
  :keymaps 'my-reference-map
  "e" (defun my-ebib-open-with-file ()
	"Open with bib file in PWD if exists otherwise use master.

If there is more than one local bib file ask."
	(interactive)
	;; REVIEW: Use a different function. Omnix may be useful to get
	;; paperwide bibs eventually.
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
	    (ebib my-refs-master-bib)))))

(with-eval-after-load 'ebib
  (customize-set-variable 'ebib-notes-directory my-refs-notes-dir)
  (customize-set-variable 'ebib-reading-list-file
			  (expand-file-name "readinglist.org" my-refs-notes-dir))
  (customize-set-variable 'ebib-default-directory my-refs-notes-dir)
  (customize-set-variable 'ebib-keywords
			  (expand-file-name ".keywords.txt" my-refs-notes-dir))
  (customize-set-variable 'ebib-notes-show-note-method nil)
  (customize-set-variable 'ebib-file-associations
			  '(("pdf" . "xdg-open")))
  (customize-set-variable 'ebib-file-search-dirs 'my-refs-stores)

  (general-def
    :keymaps 'ebib-index-mode-map
    "/" 'ebib-search)

  (my-local-leader-def
    :keymaps 'ebib-index-mode-map
    "c" 'ebib-dependent-create-dependent
    "a" 'ebib-dependent-add-entry
    "d" 'ebib-dependent-delete-entry
    "m" 'ebib-dependent-switch-to-main)

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

  (setq ebib-reading-list-template
	"* %M [[file:%k.org][%t]]\n:PROPERTIES:\n:AUTHOR: %A\n:END:\ncite:%k\n\n")
  (setq ebib-notes-template "#+TITLE: %t\n#+AUTHOR: %A\ncite:%k\n\n>|<"))

(provide 'my-references)
;;; my-references.el ends here
