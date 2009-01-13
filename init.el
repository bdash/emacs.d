(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

; Tell emacs in two different ways that it may need to look in /usr/local/bin to find git
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(require 'cl)
(require 'webkit)
(require 'webkit-merging)



; (add-to-list 'load-path "~/Documents/Source/CVS/slime")
; (require 'slime)
;
; (setf common-lisp-hyperspec-root "file:/Volumes/Data/Users/mrowe/Downloads/HyperSpec-7-0/HyperSpec/")
; (setq inferior-lisp-program "/usr/local/bin/sbcl")
;
; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
; 
; (setq lisp-indent-function 'common-lisp-indent-function
;      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
