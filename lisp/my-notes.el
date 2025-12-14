;;; my-notes.el --- For note-taking -*- lexical-binding: t -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: October 26, 2020

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
;; Org-mode config for note taking. Based primarily on org-roam.

;;; Code:

(require 'my-variables)
(require 'my-keybindings)

(defvar my-note-naming-format "%Y%m%d%H%M%S")

(defvar my-dailies-map (make-sparse-keymap))
(defvar my-roam-map (make-sparse-keymap))

(my-local-leader-def
  :keymaps 'org-mode-map
  "d" '(:keymap my-dailies-map :which-key dailies)
  "m" '(:keymap my-roam-map :which-key dailies))

(dolist (fn '(org-roam-capture
	      org-roam-node-find
	      org-roam-node-insert
	      org-roam-buffer-toggle
	      org-roam-buffer-display-dedicated))
  (autoload fn "org-roam" nil t))
(general-def
  :keymaps 'my-roam-map
  "x" 'org-roam-capture
  "f" 'org-roam-node-find
  "i" 'org-roam-node-insert
  "m" 'org-roam-buffer-toggle
  "M" 'org-roam-buffer-display-dedicated
  "c" 'org-id-get-create
  "aa" 'org-roam-alias-add
  "ar" 'org-roam-ref-add)

(general-def
  :keymaps 'my-notes-map
  "f" 'org-roam-node-find
  "x" 'org-roam-capture
  "r" 'org-roam-node-random)

(autoload 'org-roam-dailies-goto-today "org-roam-dailies" nil t)
(general-def
  :keymaps 'my-dailies-map
  "d" 'org-roam-dailies-goto-today
  "n" 'org-roam-dailies-goto-next-note
  "p" 'org-roam-dailies-goto-previous-note)

(with-eval-after-load 'org-roam
  (customize-set-variable 'org-roam-directory my-zettle-dir)
  (customize-set-variable 'org-roam-db-gc-threshold (expt 2 30))
  (customize-set-variable 'org-roam-db-location
			  (expand-file-name "org-roam.db" my-var-dir))

  (org-roam-db-autosync-mode)

  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . frame-height)))

  (require 'magit-section)
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

  (customize-set-variable 'org-roam-completion-everywhere t)
  (customize-set-variable
   'org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head (format "%%<%s>.org" my-note-naming-format)
			 "#+TITLE: ${title}\n\n- tags :: ")
      :jump-to-captured t
      :unnarrowed t)
     ("i" "immediate" plain "%?"
      :target (file+head (format "%%<%s>.org" my-note-naming-format)
			 "#+TITLE: ${title}\n")
      :unnarrowed t
      :immediate-finish t)
     ("f" "fleeting" entry "* ${title}%?"
      :target (node "Inbox")
      :unnarrowed t)))

  (require 'org-roam-bibtex)
  (customize-set-variable 'orb-roam-ref-format 'org-cite)
  (customize-set-variable 'orb-insert-link-description 'citation-org-cite)
  (customize-set-variable 'org-roam-mode-sections
			  (list #'org-roam-backlinks-section
				#'orb-section-abstract
				#'org-roam-reflinks-section))

  (with-eval-after-load 'citar
    (require 'citar-org-roam)

    (add-to-list
     'org-roam-capture-templates
     '("r" "reference" plain "%?"
       :target (file+head "references/${citar-citekey}.org"
			  "#+TITLE: ${citar-title}\n#+AUTHOR: ${citar-author}\n#+YEAR: ${citar-date}\n")
       :jump-to-captured t
       :unnarrowed t))

    (customize-set-variable
     'citar-notes-sources
     '((citar-file :name "Notes"
		   :category file
		   :items citar-file--get-notes
		   :hasitems citar-file--has-notes
		   :open find-file
		   :create orb-citar-edit-note
		   :transform file-name-nondirectory)))

    (customize-set-variable 'citar-org-roam-capture-template-key "r")
    (citar-org-roam-mode))

  (with-eval-after-load 'org-roam-dailies
    (customize-set-variable 'org-roam-dailies-directory "dailies/")
    (customize-set-variable 'org-roam-dailies-capture-templates
			    '(("d" "default" entry "* %?"
			       :target (file+head "%<%Y%m%d>.org"
						  "#+TITLE: %<%Y-%m-%d>\n"))))))

(autoload 'deft "deft")
(general-def
  :keymaps 'my-notes-map
  "d" 'deft)

(with-eval-after-load 'deft
  (defun deft-parse-title (file contents)
    "Help Deft find an Org FILE's title.

The original `deft-parse-title' assumes the title is on the first line of
CONTENTS. With `org-roam' files, the title is below some other metadata."
    (if deft-use-filename-as-title
	(deft-base-filename file)

      (let ((begin (string-match "^#\\+TITLE:.*$" contents)))
	(if begin
	    (funcall deft-parse-title-function
		     (substring contents begin (match-end 0)))))))

  (setq deft-default-extension "org")
  (customize-set-variable 'deft-new-file-format my-note-naming-format)
  (customize-set-variable 'deft-use-filter-string-for-filename nil)
  (customize-set-variable 'deft-directory my-zettle-dir)

  (general-imap
    :keymaps 'deft-mode-map
    "C-o" 'deft-open-file-other-window
    "C-w" 'deft-filter-decrement-word
    "C-n" 'next-line
    "C-p" 'previous-line))

(provide 'my-notes)
;;; my-notes.el ends here
