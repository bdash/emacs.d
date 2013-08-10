(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)

(require 'cl)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

; Tell emacs in two different ways that it may need to look in /usr/local/bin to find git
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path exec-directory)

(when (require 'package nil t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)

  (when (not package-archive-contents)
    (package-refresh-contents))

  (defvar my-packages '(color-theme
                        clojure-mode
                        clojure-test-mode
                        nrepl
                        paredit
                        rainbow-delimiters
                        auto-complete
                        ac-nrepl
                        magit
                        git-commit-mode))
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(when (>= emacs-major-version 24)
  (load-theme 'wombat t)

  ;; Wombat's background color is too light in transparent Terminal windows, so darken it up.
  (unless (display-graphic-p)
    (set-face-background 'default "#0a0a0a")))

(ido-mode)

(setq-default indent-tabs-mode nil
              make-backup-files nil
              split-width-threshold nil)

(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'nrepl-mode-hook 'show-paren-mode)

(when (or (featurep 'paredit-mode) (featurep 'paredit-mode-autoloads))
  (add-hook 'nrepl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(autoload  'auto-complete-mode "auto-complete-config")
(add-hook 'nrepl-mode-hook 'auto-complete-mode)
(add-hook 'clojure-mode-hook 'auto-complete-mode)

(when (or (featurep 'rainbow-delimiters) (featurep 'rainbow-delimiters-autoloads))
  (add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(eval-after-load "rainbow-delimiters"
  ;; Have nested delimiters use increasingly lighter shaders of yellow-gray.
  '(dolist (i (number-sequence 1 9))
     (set-face-foreground (intern (rainbow-delimiters-depth-face i))
                          (let ((c (+ ?\x40 (* i 8))))
                            (format "#%X%X%X" c c ?\x30)))))

(when (or (featurep 'ac-nrepl) (featurep 'rainbow-delimiters-autoloads))
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup))

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'nrepl-mode)

     (defun set-auto-complete-as-completion-at-point-function ()
       (setq completion-at-point-functions '(auto-complete)))
     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

     (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)))

(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG.edit\\'" . git-commit-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(add-to-list 'magic-mode-alist
                `(,(lambda ()
                     (and (string= (file-name-extension buffer-file-name) "h")
                          (re-search-forward "@\\<interface\\>" 
                                                  magic-mode-regexp-match-limit t)))
                  . objc-mode))

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
