(defun strip-trailing-new-line (str)
  (if (string= (substring str -1) "\n")
      (substring str 0 -1)
    str))

(defun dos-to-unix-path (dos-path)
  "Convert a DOS-style path that Emacs uses into a Unix-style path used by Cygwin.

Assumes that Cygwin is installed in \\cygwin on the boot volume."
  (interactive "")
  (if mswindows-p
      (let ((unix-path (shell-command-to-string (concat "\\cygwin\\bin\\cygpath -u " dos-path))))
        (strip-trailing-new-line unix-path))
    dos-path))

(defun run-unix-command (command)
  "Run the given command string under a Unix shell.  On Windows, this will use a Cygwin version of bash."
  (when mswindows-p
    (setq command (format "\\cygwin\\bin\\bash -i -l -c \"%s\"" command)))
  (shell-command-to-string command))

(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))

(defun system-name ()
  "Return the hostname of the system that we are running on, or <unknown> if we're unable to retrieve the name."
  (if macosx-p
      (strip-trailing-new-line (run-unix-command "scutil --get ComputerName"))
    "<unknown>"))

(provide 'util)
