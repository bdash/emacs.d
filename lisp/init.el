(require 'cl-lib)

(mapc (lambda (fn) (when (fboundp fn)
			   (funcall fn -1)))
	'(tool-bar-mode menu-bar-mode scroll-bar-mode))

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/lisp/")

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

  (setq package-enable-at-startup nil)
  (package-initialize)
  (unless (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)))

(use-package color-theme
  :ensure t
  :config
  (load-theme 'wombat t)
  ;; Wombat's background color is too light in transparent Terminal windows, so darken it up.
  (unless (display-graphic-p)
    (set-face-background 'default "#0a0a0a")))

(use-package ido
  :config
  (ido-mode))
(use-package idomenu :ensure t)
(use-package ido-completing-read+ :ensure t)

(use-package markdown-mode+
  :ensure t
  :commands markdown-mode
  :mode ("\\.md$" . markdown-mode))

(use-package smartparens
  :ensure t
  :commands smartparens-mode show-smartparens-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'show-smartparens-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-mode)
  (add-hook 'cider-repl-mode-hook 'show-smartparens-mode)
  :config
  (use-package smartparens-config)
  (sp-use-paredit-bindings)
  (add-hook 'smartparens-mode-hook 'smartparens-strict-mode)
  (set-face-background 'sp-show-pair-match-face nil)
  (set-face-foreground 'sp-show-pair-match-face "#cae682")
  (setq sp-highlight-pair-overlay nil))

(use-package clojure-mode
  :ensure t
  :mode "\\.clj$"
  :config)

(use-package cider
  :ensure t
  :defer t
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

  :config
  ;; Have nested delimiters use increasingly lighter shaders of yellow-gray.
  (dolist (i (number-sequence 1 9))
    (set-face-foreground (intern (format "rainbow-delimiters-depth-%d-face" i))
                          (let ((c (+ ?\x40 (* i 12))))
                            (format "#%X%X%X" c c ?\x30)))))

(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-c SPC" . ace-jump-mode)
  ("C-c C-SPC" . ace-jump-mode))

(use-package find-file
  :commands ff-find-other-file
  :init
  (add-hook 'c-mode-common-hook '(lambda () (local-set-key "\C-ct" 'ff-find-other-file)))
  :config
  (add-to-list 'cc-other-file-alist '("\\.m\\'" (".h")))
  (add-to-list 'cc-other-file-alist '("\\.mm\\'" (".h" ".hpp")))
  (add-to-list 'cc-other-file-alist '("\\.h\\'" (".m" ".mm" ".cpp"))))

(use-package flycheck-irony
  :ensure t
  :commands flycheck-irony-setup)

(use-package irony
  :ensure t
  :commands irony-mode irony--find-server-executable
  :init
  (defun enable-irony-mode-if-server-available ()
    (when (irony--find-server-executable)
      (irony-mode)))
  (add-hook 'c-mode-common-hook 'enable-irony-mode-if-server-available)

  :config
  (when (irony--find-server-executable)
     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
     (eval-after-load 'company
       '(add-to-list 'company-backends 'company-irony))
     (eval-after-load 'flycheck
       '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))))

(use-package company
  :ensure t
  :demand t
  :bind ("\M-/" . company-complete-common)
  :config (global-company-mode))

(use-package company-irony
  :ensure t
  :commands company-irony)

(use-package flycheck
  :ensure t
  :demand t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line)
        flycheck-display-errors-delay 0.01)
  (global-flycheck-mode))

(use-package magit
  :ensure t
  :commands magit-mode
  :config
  (when (not window-system)
    (set-face-foreground 'magit-diff-added "green3")
    (set-face-foreground 'magit-diff-removed "red3")
    (set-face-foreground 'magit-diff-added-highlight "green3")
    (set-face-foreground 'magit-diff-removed-highlight "red3")
    (set-face-background 'magit-diff-added "gray10")
    (set-face-background 'magit-diff-removed "gray10")
    (set-face-background 'magit-diff-added-highlight "gray10")
    (set-face-background 'magit-diff-removed-highlight "gray10")))

(use-package git-commit
  :ensure t
  :config
  :mode ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" . git-commit-mode))

(use-package cmake-mode
  :ensure t
  :mode "\\(/CMakeLists.txt|\\.cmake\\)$"
  :config
  (setq cmake-tab-width 4))

(use-package rust-mode
  :ensure t
  :mode "\\.rs$")

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(add-to-list 'magic-mode-alist
                `(,(lambda ()
                     (and (string= (file-name-extension buffer-file-name) "h")
                          (re-search-forward "@\\<interface\\>"
                                                  magic-mode-regexp-match-limit t)))
                  . objc-mode))

(defconst webkit-cc-style
  '("user"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "webkit" webkit-cc-style)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (setq tab-width 8
                   c-basic-offset 4
                   c-default-style "webkit")))


(setq-default indent-tabs-mode nil
              make-backup-files nil
              split-width-threshold nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fill-column 120)
 '(ns-antialias-text t)
 '(ns-use-qd-smoothing nil)
 '(package-selected-packages
   (quote
    (irony typescript-mode irony-eldoc use-package smartparens rust-mode rainbow-delimiters markdown-mode+ magit idomenu ido-completing-read+ flycheck-irony company-irony color-theme cmake-mode cider ace-jump-mode)))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values (quote ((c-file-offsets (innamespace . 0)))))
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

(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(require 'util)

(defvar per-host-init-file (concat (file-name-directory (locate-library "init")) "hosts/" (system-name) ".el"))

(if (file-exists-p per-host-init-file)
    (load per-host-init-file))
