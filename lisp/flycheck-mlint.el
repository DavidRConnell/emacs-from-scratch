;;; flycheck-mlint.el --- Flycheck checker using Matlab mlint -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Andrzej Pronobis
;; Author: Andrzej Pronobis <a.pronobis@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is an extension for Flycheck.
;; To use, add the following to your init:
;; (eval-after-load 'flycheck
;;   '(require 'flycheck-matlab-mlint))

;;; Code:

(require 'flycheck)

(defvar flycheck-matlab-mlint-executable-path
  ""
  "Default full executable path.")

(flycheck-def-executable-var matlab-mlint "<platform>/mlint")

(flycheck-define-command-checker 'matlab-mlint
  "A Matlab checker based on mlint."
  :command `(,flycheck-matlab-mlint-executable-path source)
  :error-patterns
  '((warning line-start "L " line " (C " (1+ digit)  "): " (message) line-end))
  :modes '(matlab-mode)
  :enabled (lambda () (string= (file-name-extension (buffer-file-name))
			       "m")))

(add-to-list 'flycheck-checkers 'matlab-mlint 'append)

(provide 'flycheck-mlint)
;;; flycheck-mlint.el ends here
