;; Functions related to merging changes between branches in WebKit

(require 'webkit)

(custom-set-variables
 '(add-log-full-name "Mark Rowe")
 '(add-log-mailing-address "mrowe@apple.com")
 '(change-log-version-number-regexp-list (list "Merge r\\\([0-9]+\\\)")))

(defun prompt-for-svn-revision ()
  (let* ((prev-revision (string-to-number (change-log-version-number-search)))
         (next-revision (+ 1 prev-revision))
         (next-revision-string (format "r%s" next-revision)))
    (read-string (format "SVN revision: ") next-revision-string)))

(defun add-change-log-merge-header (&optional svn-revision)
  (interactive (list (prompt-for-svn-revision)))
  (let* ((defun (add-log-current-defun))
         bound
         (full-name (or add-log-full-name (user-full-name)))
         (mailing-address (or add-log-mailing-address user-mail-address)))

    (or (eq major-mode 'change-log-mode)
        (change-log-mode))
    (undo-boundary)
    (goto-char (point-min))

    (let ((new-entries
           (mapcar (lambda (addr)
                     (concat
                      (if (stringp add-log-time-zone-rule)
                          (let ((tz (getenv "TZ")))
                            (unwind-protect
                                (progn
                                  (set-time-zone-rule add-log-time-zone-rule)
                                  (funcall add-log-time-format))
                              (set-time-zone-rule tz)))
                        (funcall add-log-time-format))
                      "  " full-name
                      "  <" addr ">"))
                   (if (consp mailing-address)
                       mailing-address
                     (list mailing-address)))))
      (if (and (not add-log-always-start-new-record)
               (let ((hit nil))
                 (dolist (entry new-entries hit)
                   (when (looking-at (regexp-quote entry))
                     (setq hit t)))))
          (forward-line 1)
        (insert (nth (random (length new-entries))
                     new-entries)
                (if use-hard-newlines hard-newline "\n")
                (if use-hard-newlines hard-newline "\n"))
        (forward-line -1)))
    (insert "\n")
    (indent-line-to 8)
    (insert (format "Merge %s." svn-revision))
    (save-excursion
      (insert "\n")
      (forward-line 1)
      (indent-line-to 4))
    (forward-char -1)
    )
  )

(defun beginning-of-re-match (re)
  (save-excursion
    (when (re-search-forward re nil t)
      (match-beginning 0))))

(defun change-log-header-start ()
  (beginning-of-re-match "^[0-9][0-9][0-9][0-9]-"))

(defun merge-header-end ()
  (beginning-of-re-match "^    [0-9][0-9][0-9][0-9]-"))

(defun entry-has-merge-header-p ()
  (goto-char (change-log-header-start))
  (forward-line 4)
  (equal (point) (merge-header-end)))

(defun buffer-has-conflict-p ()
  (save-excursion (re-search-forward "^<<<<<<<<" nil t)))

(defun incorrect-merge-p ()
  (interactive "")
  (save-excursion 
    (goto-char (point-min))

    ;; We can't deal with incorrect merges until conflicts have been resolved
    (unless (buffer-has-conflict-p)

      ;; An incorrect merge has a merge header on the first change log entry
      ;; but has none on the second
      (and (entry-has-merge-header-p)
           (not (entry-has-merge-header-p))))))
  
(defun fix-incorrect-merge ()
  (interactive "")

  ;; If we don't have an incorrect merge in this buffer, we've probably not
  ;; reloaded the file from disk since the merge took place.
  (unless (incorrect-merge-p)
    (revert-buffer t t t))

  (when (incorrect-merge-p)
    (goto-char (point-min))
    (let* ((bad-header-end (merge-header-end))
           (bad-header-text (buffer-substring (point-min) bad-header-end)))

      ;; Remove the merge header
      (delete-region (point-min) bad-header-end)

      ;; Correct the indentation on the header
      (indent-line-to 0)

      ;; Insert the merge header at the correct location
      (forward-line)
      (goto-char (change-log-header-start))
      (beginning-of-line)
      (insert bad-header-text)
      (indent-line-to 4)

      ;; Add a new merge header above the first entry
      (add-change-log-merge-header (prompt-for-svn-revision))
      )))

(provide 'webkit-merging)