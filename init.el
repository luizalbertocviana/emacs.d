;; startup preparations
(defvar last-file-name-handler-alist file-name-handler-alist)
;; relax garbage collector a little
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      ;; avoid regex search for el and elc files loaded during startup
      file-name-handler-alist nil)

;; substitute "yes or no" prompts for "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; auto-answer yes during startup (needed to build some packages)
(fset 'old-prompt (symbol-function 'y-or-n-p))
(fset 'y-or-n-p (lambda (&rest args) t))

;; directory containing the following files
(add-to-list 'load-path user-emacs-directory)

;; setting some options
(load "options")

;; user information
(load "user")

;; start straight package manager
(load "init-straight")

;; commonly used packages
(load "common")

;; custom functions (needs to be loaded after common to override some
;; default functions)
(load "functions")

;; modeline setup
(load "modeline")

;; evil setup
(load "evil-setup")

;; keybindings setup
(load "keybindings")

;; org setup
(load "org")

;; gnus setup
;; (load "gnus-setup")

;; LaTeX setup
(load "latex")

;; cpp setup
(load "cpp-setup")

;; clojure setup
;; (load "clojure-setup")

;; common lisp setup
;; (load "common-lisp")

;; cmake setup
(load "cmake")

;; python setup
(load "python-setup")

;; hy setup
;; (load "hy")

;; haskell setup
(load "haskell-setup")

;; rust setup
(load "rust-setup")

;; after initialization, revert startup preparations
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist last-file-name-handler-alist)

;; restore manual answer prompt
(fset 'y-or-n-p 'old-prompt)
