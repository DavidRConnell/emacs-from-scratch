;;; my-eww.el --- Refuge for random apps -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: December 09, 2025

;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; Add eww readable to improve eww display of web pages.

;;; Code:

(require 'my-keybindings)

(setq eww-readable-urls '(".*"))

(require 'eww)

(require 'pixel-scroll)
(require 'cl-lib)
(require 'dom)
(require 'subr-x)
(require 'url-util)

(setq browse-url-browser-function 'eww-browse-url)
(setq shr-use-fonts nil
      shr-max-width 79)

(my-local-leader-def
  :keymaps 'eww-mode-map
  "i" 'eww-toggle-images
  "y" 'eww-copy-page-url
  "b" 'eww-list-buffers
  "r" 'my-eww-add-doi-to-bib)

(general-nmmap
  :keymaps 'eww-mode-map
  "C-o" 'eww-back-url
  "C-i" 'eww-forward-url
  "C-j" 'eww-next-url
  "C-k" 'eww-previous-url)

(general-nmmap
  :keymaps 'override
  :predicat '(derived-mode-p 'eww-mode)
  "J" 'my-eww-scroll-line-down
  "K" 'my-eww-scroll-line-up)

(defun my-eww-scroll-line-down ()
  (interactive)
  (pixel-scroll-precision-scroll-down (frame-char-height)))

(defun my-eww-scroll-line-up ()
  (interactive)
  (pixel-scroll-precision-scroll-up (frame-char-height)))

(defun my--eww-doi-candidates ()
  "Return unique DOIs linked from the current EWW DOM."
  (let ((dom (plist-get eww-data :dom))
        (case-fold-search t)
        dois)
    (when dom
      (dolist (node (dom-by-tag dom 'a))
        (let ((href (string-trim (or (dom-attr node 'href) ""))))
          (when (string-match
                 (concat "\\`\\(?:https?:\\)?//\\(?:dx\\.\\)?doi\\.org/"
                         "\\([^?#]+\\)")
                 href)
            (let ((doi (url-unhex-string (match-string 1 href))))
              (when (string-match-p
                     "\\`10\\.[0-9]\\{4,9\\}/[^[:space:]]+\\'" doi)
                (push doi dois)))))))
    (delete-dups (nreverse dois))))

(defun my-eww-add-doi-to-bib ()
  (interactive)
  (let ((canidates (my--eww-doi-candidates)))
    (unless canidates
      (user-error "No DOI found on current page"))
    (let ((doi (completing-read "Article DOI: " canidates nil t)))
      (doi-utils-add-bibtex-entry-from-doi doi))))

(defun my-eww-heading-face-p (face)
  "Return non-nil if FACE includes an `shr' heading face."
  (let ((faces (if (listp face) face (list face))))
    (cl-some (lambda (f)
               (memq f '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6)))
             faces)))

(defun my-eww-imenu-create-index ()
  "Generate an Imenu index for headings in `eww-mode' buffers."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (back-to-indentation)
        (let ((face (get-text-property (point) 'face)))
          (when (my-eww-heading-face-p face)
            (let ((title (string-trim (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position))))
                  (pos (line-beginning-position)))
              (unless (string-empty-p title)
                (push (cons title pos) index)))))
        (forward-line 1)))
    (nreverse index)))

(add-hook 'eww-mode-hook
          (lambda ()
            (setq-local imenu-create-index-function #'my-eww-imenu-create-index)))

(provide 'my-eww)
;;; my-eww.el ends here
