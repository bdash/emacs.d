(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

; Tell emacs in two different ways that it may need to look in /usr/local/bin to find git
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(require 'cl)
(require 'webkit)
(require 'webkit-merging)
(ido-mode)

(setq-default indent-tabs-mode nil
              make-backup-files nil)

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

(add-to-list 'load-path "~/Documents/Source/CVS/slime")
(require 'slime-autoloads)

(eval-after-load "slime"
  '(progn
    (add-to-list 'load-path "/usr/local/slime/contrib")
    (slime-setup '(slime-fancy slime-banner))
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

; (setf common-lisp-hyperspec-root "file:/Volumes/Data/Users/mrowe/Downloads/HyperSpec-7-0/HyperSpec/")
(setq sbcl-source-root "/Volumes/Data/Users/mrowe/Documents/Source/Git/sbcl/")
(setenv "SBCL_HOME" (concat sbcl-source-root "contrib"))
(setq inferior-lisp-program (concat sbcl-source-root "src/runtime/sbcl --core " sbcl-source-root "output/sbcl.core"))

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

; (setq lisp-indent-function 'common-lisp-indent-function
;       slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

    (defun slime-description-fontify ()
      "Fontify sections of SLIME Description."
      (with-current-buffer "*SLIME Description*"
        (highlight-regexp
         (concat "^Function:\\|"
                 "^Macro-function:\\|"
                 "^Its associated name.+?) is\\|"
                 "^The .+'s arguments are:\\|"
                 "^Function documentation:$\\|"
                 "^Its.+\\(is\\|are\\):\\|"
                 "^On.+it was compiled from:$")
         'hi-green-b)))
    (defadvice slime-show-description (after slime-description-fontify activate)
      "Fontify sections of SLIME Description."
      (slime-description-fontify))
