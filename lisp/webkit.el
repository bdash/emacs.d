;; Functions to make life with WebKit easier

;; ChangeLogs use 4 space indentation
(setq change-log-mode-hook '(lambda ()
                              (setq indent-tabs-mode nil)
                              (setq tab-width 4)))

(defun find-webkit-root ()
  "Find the root directory of the WebKit tree that contains the current buffer."
  (interactive "")
  (let ((last-directory buffer-file-name)
        (current-directory (file-name-directory buffer-file-name)))
    (while (and (not (equalp last-directory current-directory))
                (not (directory-files current-directory nil "OpenSource")))
      (setq last-directory current-directory)
      (setq current-directory (expand-file-name (file-name-as-directory (concat current-directory "..")))))
    (if (equalp last-directory current-directory)
        (error "Unable to find WebKit root directory"))
    (file-name-as-directory (concat current-directory "OpenSource"))))

(defun resolve-change-log-conflicts ()
  "Run resolve-ChangeLogs on the current buffer."
  (interactive)
  (let* ((dos-webkit-root (find-webkit-root))
         (webkit-root (dos-to-unix-path dos-webkit-root))
         (resolve-change-log-script (concat webkit-root "WebKitTools/Scripts/resolve-ChangeLogs"))
         (unix-buffer-file-name (dos-to-unix-path buffer-file-name))
         (unix-buffer-directory-name (file-name-directory unix-buffer-file-name))
         (unix-buffer-relative-file-name (file-name-nondirectory unix-buffer-file-name))
         (command (format "cd %s && %s %s 2>&1" unix-buffer-directory-name resolve-change-log-script unix-buffer-relative-file-name))
         (result (strip-trailing-new-line (run-unix-command command))))
    (revert-buffer t t t)
    (message "%s" result)
    (goto-char (point-min))))

(provide 'webkit)