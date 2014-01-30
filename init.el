(require 'cl)

(mapc (lambda (fn) (when (fboundp fn)
			   (funcall fn -1)))
	'(tool-bar-mode menu-bar-mode scroll-bar-mode))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

(if (not (getenv "TERM_PROGRAM"))
    (let ((path (shell-command-to-string
                 "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
      (setenv "PATH" path)))


(when (require 'package nil t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (defvar my-packages '(color-theme
                        clojure-mode
                        clojure-test-mode
                        cider
                        paredit
                        rainbow-delimiters
                        auto-complete
                        ac-nrepl
                        magit
                        git-commit-mode
                        ace-jump-mode
                        ido-ubiquitous
                        idomenu))
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(when (>= emacs-major-version 24)
  (load-theme 'wombat t)

  ;; Wombat's background color is too light in transparent Terminal windows, so darken it up.
  (unless (display-graphic-p)
    (set-face-background 'default "#0a0a0a")))

(ido-mode)
(when (require 'ido-ubiquitous nil t)
  (ido-ubiquitous))

(setq-default indent-tabs-mode nil
              make-backup-files nil
              split-width-threshold nil)

(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'cider-mode-hook 'show-paren-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(when (or (featurep 'paredit) (featurep 'paredit-autoloads))
  (add-hook 'cider-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(autoload  'auto-complete-mode "auto-complete-config")
(add-hook 'cider-mode-hook 'auto-complete-mode)
(add-hook 'clojure-mode-hook 'auto-complete-mode)

(when (or (featurep 'rainbow-delimiters) (featurep 'rainbow-delimiters-autoloads))
  (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(eval-after-load "rainbow-delimiters"
  ;; Have nested delimiters use increasingly lighter shaders of yellow-gray.
  '(dolist (i (number-sequence 1 9))
     (set-face-foreground (rainbow-delimiters-depth-face i)
                          (let ((c (+ ?\x40 (* i 12))))
                            (format "#%X%X%X" c c ?\x30)))))

(when (or (featurep 'ac-nrepl) (featurep 'rainbow-delimiters-autoloads))
  (add-hook 'cider-mode-hook 'ac-nrepl-setup)
  (add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup))

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)

     (defun set-auto-complete-as-completion-at-point-function ()
       (setq completion-at-point-functions '(auto-complete)))
     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

     (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'cider-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)))

(when (or (featurep 'ace-jump-mode) (featurep 'ace-jump-mode-autoloads))
  (progn
     (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
     (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)))

(autoload 'idomenu "idomenu" nil t)

(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG.edit\\'" . git-commit-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(add-to-list 'magic-mode-alist
                `(,(lambda ()
                     (and (string= (file-name-extension buffer-file-name) "h")
                          (re-search-forward "@\\<interface\\>" 
                                                  magic-mode-regexp-match-limit t)))
                  . objc-mode))

(require 'find-file)
(add-to-list 'cc-other-file-alist '("\\.m\\'" (".h")))
(add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h")))
(add-to-list 'cc-other-file-alist '("\\.h\\'" (".m" ".mm")))
(add-hook 'c-mode-common-hook '(lambda () (local-set-key "\C-ct" 'ff-find-other-file)))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq tab-width 8
                   c-basic-offset 4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-log-full-name "Mark Rowe")
 '(add-log-mailing-address "mrowe@apple.com")
 '(change-log-version-number-regexp-list (list "Merge r\\([0-9]+\\)"))
 '(column-number-mode t)
 '(fill-column 120)
 '(ns-antialias-text t)
 '(ns-use-qd-smoothing nil)
 '(size-indication-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-handled-backends (quote nil)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100))))
 '(mode-line ((t nil))))

(load "term/xterm") 

(defun terminal-init-screen () 
   "Terminal initialization function for screen." 
   ;; Use the xterm color initialization code. 
   (xterm-register-default-colors) 
   (tty-set-up-initial-frame-faces)) 

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(require 'util)
(require 'webkit)
(require 'webkit-merging)

(defvar per-host-init-file (concat (file-name-directory (locate-library "init")) "hosts/" (system-name) ".el"))

(if (file-exists-p per-host-init-file)
    (load per-host-init-file))
