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

(require 'my-variables)
(require 'my-keybindings)

(setq elfeed-search-filter "@4-week-ago +unread")
(autoload 'elfeed "elfeed")

(my-leader-def
  "E" 'elfeed)

(with-eval-after-load 'elfeed
  (setq rmh-elfeed-org-files `(,(expand-file-name "elfeed.org" my-notes-dir)))

  (require 'elfeed-org)

  (general-nmmap
    :keymaps 'elfeed-show-mode-map
    "C-j" 'elfeed-show-next
    "C-k" 'elfeed-show-prev)

  (general-def
    :keymaps 'elfeed-search-mode-map
    "&" 'elfeed-search-browse-url)

  (set-face-attribute 'elfeed-search-title-face nil :weight 'light)
  (elfeed-org))

(provide 'my-rss)
;;; my-rss.el ends here
