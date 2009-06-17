(add-to-list 'load-path "~/Documents/Source/Git/magit")
(require 'magit)

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
