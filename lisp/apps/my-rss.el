;;; my-rss.el --- RSS feeds -*- lexical-binding: t; -*-

;; Copyright (C) 2020 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: July 28, 2020

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
;; Manage RSS feeds.

;;; Code:

(use-package elfeed
  :general
  (my-leader-def
    "E" #'elfeed)

  :config
  (general-define-key
   :keymaps 'elfeed-search-mode-map
   ;; "r" #'elfeed-update
   "&" #'elfeed-search-browse-url)

  (setq elfeed-search-filter "@2-week-ago"
	elfeed-show-entry 'display-buffer)

  (set-face-attribute 'elfeed-search-title-face nil :weight 'light)
  (add-hook 'elfeed-show-mode-hook #'visual-line-mode)

  (use-package elfeed-org
    :init
    (setq rmh-elfeed-org-files (list "~/notes/elfeed.org"))
    :config
    (elfeed-org)))

(use-package elpher
  :commands elpher
  :config
  (general-nmmap
    :keymaps 'elpher-mode-map
    "C-o" #'elpher-back
    "C-c C-o" #'elpher-follow-current-link
    "C-n" #'elpher-next-link
    "C-p" #'elpher-prev-link)
  (setq elpher-start-page-url "gopher://gopher.floodgap.com/7/v2/vs"))

(provide 'my-rss)
;;; my-rss.el ends here
