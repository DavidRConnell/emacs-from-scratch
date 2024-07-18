;;; flymake-codeissues.el --- A flymake backend for MATLAB's codeIssues -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 David R. Connell
;;
;; Maintainer: David R. Connell <david32@dcon.addy.io>
;; Created: July 17, 2024
;; Modified: July 17, 2024
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; creates a flymake backend for MATLAB's code analyzer codeIssues.
;; Requires a running MATLAB process.
;;; Code:

(require 'flymake)

(defun matlab--flymake-matlab-shell-p ()
  "Test if there is a MATLAB process currently running."
  (if (seq-filter #'(lambda (x) (string= (process-name x) "MATLAB"))
		  (process-list))
      t nil))

(defun matlab--flymake-severity (level)
  "Convert a LEVEL string to a flymake type."
  (cond ((string= level "info") :note)
	((string= level "warning") :warning)
	(t :error)))

(defun matlab-codeissues-flymake (report-fn &rest _args)
  "Flymake backend for MATLAB's codeIssues code analyzer."
  (if (and (matlab--flymake-matlab-shell-p) (json-available-p))
      (let ((issues (json-parse-string
		     (matlab-shell-collect-command-output
		      (format "disp(jsonencode(codeIssues('%s').Issues))"
			      (buffer-file-name)))
		     :array-type 'list)))
	(funcall report-fn
		 (mapcar #'(lambda (ht) (flymake-make-diagnostic
					 (get-file-buffer
					  (gethash "FullFilename" ht))
					 (cons (gethash "LineStart" ht)
					       (gethash "ColumnStart" ht))
					 (cons (gethash "LineEnd" ht)
					       (gethash "ColumnEnd" ht))
					 (matlab--flymake-severity
					  (gethash "Severity" ht))
					 (gethash "Description" ht)))
			 issues)))))

(defun matlab-setup-flymake-backend ()
  "Setup matlab flymake backend."
  (add-hook #'flymake-diagnostic-functions #'matlab-codeissues-flymake nil t))

(add-hook 'matlab-mode-hook 'matlab-setup-flymake-backend)

(provide 'flymake-codeissues)
;;; flymake-codeissues.el ends here
