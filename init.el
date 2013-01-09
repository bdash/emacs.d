(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

; Tell emacs in two different ways that it may need to look in /usr/local/bin to find git
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

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
                        ac-nrepl))
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(when (>= emacs-major-version 24)
  (load-theme 'wombat t))

(ido-mode)

(setq-default indent-tabs-mode nil
              make-backup-files nil
              split-width-threshold nil
              nrepl-popup-stacktraces nil)

(when (require 'paredit nil t)
  (add-hook 'nrepl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(when (require 'auto-complete nil t)
  (add-hook 'nrepl-mode-hook 'auto-complete-mode)
  (add-hook 'clojure-mode-hook 'auto-complete-mode))

(when (require 'rainbow-delimiters nil t)
  (add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(when (require 'ac-nrepl nil t)
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'nrepl-mode))

  (defun set-auto-complete-as-completion-at-point-function ()
    (setq completion-at-point-functions '(auto-complete)))
  (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

  (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function))



(defun my-c-mode-common-hook ()
  (setq tab-width 8
        c-basic-offset 4))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

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
 '(default ((t (:height 100)))))

(set-face-background 'region "grey20")

(require 'util)
(require 'cl)
(require 'webkit)
(require 'webkit-merging)

(defvar per-host-init-file (concat (file-name-directory (locate-library "init")) "hosts/" (system-name) ".el"))
(message per-host-init-file)

(if (file-exists-p per-host-init-file)
    (load per-host-init-file))
