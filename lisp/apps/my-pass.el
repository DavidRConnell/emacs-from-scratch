;;; my-pass.el --- Use pass for authentication -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 David R. Connell
;;
;; Author: David R. Connell <davidconnell12@gmail.com>
;; Created: January 15, 2023
;;
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
;; Set up auth-source to use my password-store.

;;; Code:

(require 'my-variables)

(require 'auth-source)

(customize-set-variable 'auth-source-pass-filename
			(expand-file-name "password-store" my-var-dir))

(auth-source-pass-enable)
(auth-source-forget-all-cached)

(provide 'my-pass)
;;; my-pass.el ends here
