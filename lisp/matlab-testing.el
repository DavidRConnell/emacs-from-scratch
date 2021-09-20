;;; matlab-testing --- aoeu
;;; Commentary:
;;; Code:

(defun mt-toggle-test-file ()
  (interactive)
  (if (string-match-p "Test" (buffer-name))
      (mt--find-original-file (projectile-project-root)
				     (mt--get-filename))
    (mt--find-test-file)))

(defun mt-get-test-dir ()
  (interactive)
  (concat (projectile-project-root) "tests/"))

(defun mt--get-filename (&optional test)
  (let ((buffer (buffer-name)))
    (string-match "\\(Test\\)?\\([a-zA-Z0-9]*?\\)\\(\\.[a-zA-Z0-9]*\\)" buffer)
    (if test
	(concat "Test" (evil-upcase-first (match-string 2 buffer)) (match-string 3 buffer))
      (concat (evil-downcase-first (match-string 2 buffer)) (match-string 3 buffer)))))

(defun mt--find-original-file (dir filename)
  (if (file-exists-p (concat dir filename))
      (find-file (concat dir filename))
    (cl-loop for f in (directory-files dir) do
	     (message f)
	     (if (not (string-match-p "\\." f))
		 (mt--find-original-file
		  (concat dir f "/") filename)))))

(defun mt--find-test-file ()
    (let* ((test-dir (mt-get-test-dir))
           (test-filename (mt--get-filename 'test))
           (test-file (concat test-dir test-filename)))

      (if (not (file-exists-p test-dir))
          (mkdir test-dir))

      (message test-file)
      (if (not (file-exists-p test-file))
          (let ((buffer (get-buffer-create test-filename)))
            (with-current-buffer (switch-to-buffer buffer)
              (matlab-mode)
              (undo-fu-only-undo)
              (evil-insert-state)
              (mt--insert-test-snippet)
              (write-file test-file)))
        (find-file test-file))))

(defun mt--insert-test-snippet ()
  "Add snippet for matlab function when opening a new .m file."
  (yas-expand-snippet (yas-lookup-snippet 'unittest 'matlab-mode)))

(defun mt-run-last-command ()
    "Run the last command sent to the matlab shell buffer."
    (interactive)
    (let ((curr-buffer (buffer-name)))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (mt-run-command (comint-previous-input-string 0))
      (switch-to-buffer curr-buffer)))

(defvar mt--history nil)
(defun mt-run-command (&optional command)
    "Send a command to the running matlab shell buffer."
    (interactive)
    (let ((curr-buffer (buffer-name)))

      (if (not mt--history)
          (progn
            (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
            (setq mt--history (mapcar 'list
                                             (cl-remove-duplicates
                                              (cddr comint-input-ring) :test #'equal)))
            (switch-to-buffer curr-buffer)))

      (if (not command)
          (progn
            (setq command (completing-read "Command: "
                                           mt--history
                                           (lambda (x) (not (member (car x)
                                                               (list "exit" nil " "))))
                                           nil))))

      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
      (matlab-shell-send-string (concat command "\n"))
      (add-to-list 'mt--history (list command))
      (matlab-shell-add-to-input-history command)
      (switch-to-buffer curr-buffer)))

(defun mt-shell-run-tests (scope tag)
  (interactive)
  (let ((test-suite "testSuite = matlab.unittest.TestSuite.from%s('%s', 'tag', '%s'); ")
	(command (concat "testResults = run(%s); "
			 "utils.summarizeTests(testResults)")))

    (cond ((string= scope "file")
	   (mt-run-command
	    (concat
	     (format test-suite
		     "File"
		     (concat (mt-get-test-dir)
			     (mt--get-filename 'test))
		     tag)
	     (format command "testSuite"))))

	  ((string= scope "project")
	   (mt-run-command
	    (concat
	     (format test-suite "Folder" (mt-get-test-dir) tag)
	     (format command "testSuite"))))

	  ((string= scope "rerun")
	   (mt-run-command
	    (concat "testSuite = testSuite([testResults.Failed]);"
		    (format command "testSuite")))))))

(defun mt-shell-run-performance-tests (scope)
  (interactive)
  (let ((command (concat "testResults = runperf('%s', 'tag', 'Performance'); "
			 "utils.summarizeTests(testResults)")))
    (cond ((string= scope "file")
	   (mt-run-command
	    (format command (concat
			     (mt-get-test-dir)
			     (mt--get-filename 'test)))))

	  ((string= scope "project")
	   (mt-run-command
	    (format command (mt-get-test-dir)))))))

(defun mt-shell-test-summary ()
  (interactive)
  (mt-run-command "utils.summarizeTests(testResults)"))

(defun mt-shell-run-line (n)
  (interactive "p")
  (let ((beg (point-at-bol))
	(end (point-at-eol n)))
    (save-excursion
      (goto-char beg)
      (push-mark-command 0)
      (goto-char end)
      (matlab-shell-run-region-or-line))
    (pop-mark)
    (evil-force-normal-state)))

(evil-define-operator mt-shell-run-region (beg end)
    :move-point nil
    :type line
    :repeat t
    :jump t
    (interactive "<r>")
    (save-excursion
      (goto-char beg)
      (push-mark-command 0)
      (goto-char end)
      (matlab-shell-run-region-or-line))
    (pop-mark)
    (evil-force-normal-state))

(provide 'matlab-testing)
;;; matlab-testing.el ends here
