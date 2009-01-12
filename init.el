(add-to-list 'load-path "~/Documents/Source/Git/git/contrib/emacs")
(add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/site-lisp/ecb-2.32")
(add-to-list 'load-path "~/Documents/Source/CVS/slime")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(add-to-list 'exec-path "/usr/local/bin")

(require 'cl)
;; (require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)

(setf common-lisp-hyperspec-root "file:/Volumes/Data/Users/mrowe/Downloads/HyperSpec-7-0/HyperSpec/")

(require 'slime)

(require 'ecb-autoloads)

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
  (save-excursion 
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
	))))

; (add-to-list 'load-path "~/Downloads/slime-2.0/")  ; your SLIME directory
(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
; (require 'slime)
; (slime-setup)
; 
; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
; 
; (setq lisp-indent-function 'common-lisp-indent-function
;      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
