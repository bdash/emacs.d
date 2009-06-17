;;; emacs-rc-cedet.el --- 

;; Copyright (C) 2003 Alex Ott
;;
;; Author: alexott@gmail.com
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(load-file "~/Documents/Source/CVS/cedet/common/cedet.el")

(semantic-load-enable-guady-code-helpers)
(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)

(setq senator-minor-mode-name "SN")
(setq semantic-imenu-auto-rebuild-directory-indexes nil)
(global-srecode-minor-mode 1)
(global-semantic-mru-bookmark-mode 1)

(require 'semantic-decorate-include)

;; gcc setup
(require 'semantic-gcc)
(semantic-gcc-setup "gcc")

;; smart complitions
(require 'semantic-ia)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local erlang-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

(require 'eassist)

;; customisation of modes
(defun my-cedet-hook ()
; (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c." 'senator-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;;
;; (setq-default semanticdb-default-save-directory "~/tmp/semantic")

;; 
(require 'semanticdb-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)


(semantic-add-system-include "~/exp/include" 'c++-mode)
(semantic-add-system-include "~/exp/include" 'c-mode)

(custom-set-variables
 '(semantic-idle-scheduler-idle-time 10)
)

(semantic-load-enable-all-exuberent-ctags-support)
(semanticdb-enable-exuberent-ctags 'c-mode)
(semanticdb-enable-exuberent-ctags 'c++-mode)

;;; ede customization
(require 'semantic-lex-spp)
(global-ede-mode t)

;; cpp-tests project definition
(ede-cpp-root-project "WebKit"
                      :file "~/Documents/Work/WebKit-git/OpenSource/ChangeLog"
                      :include-path '("JavaScriptCore" "JavaScriptCore/wtf")
                      :system-include-path '())

;; to look 
;; (setq path-to-makefiles (make-hash-table :test 'eq))

;; (setq cpp-test-prj (ede-cpp-root-project "cpp-tests" :file "~/projects/lang-exp/cpp/CMakeLists.txt"
;;                                         :system-include-path '("/home/ott/exp/include"
;;                                                                "/home/ott/exp/include/boost-1_37"))
;;      )

;; (puthash cpp-test-prj "make -k -C ~/projects/lang-exp/cpp/build" path-to-makefiles)

;; (defun my-compile ()
;;  "compilation using ede projects"
;;  (interactive)
;;  (setq current-dir (file-name-directory (buffer-file-name (current-buffer))))
;;  (setq prj (ede-current-project current-dir))
;;  (when prj
;;    (setq make-cmd (gethash prj path-to-makefiles))
;;    (save-buffer)
;;    (save-some-buffers)
;;    (compile (format "%s" make-cmd))
;;    )
;;  )

;;; emacs-rc-cedet.el ends here
