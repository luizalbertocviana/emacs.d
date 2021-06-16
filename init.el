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
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

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

;; ivy setup
(load "ivy-setup")

;; org setup
(load "org")

;; vterm (a better terminal emulator for emacs)
;; (load "vterm")

;; gnus setup
;; (load "gnus-setup")

;; LaTeX setup
(load "lang/latex-auctex")
;; (load "lang/latex-auctex-lsp")

;; cpp setup
(load "lang/cpp")

;; clojure setup
(load "lang/clojure")

;; common lisp setup
(load "lang/lisp")

;; cmake setup
(load "lang/cmake")

;; python setup
(load "lang/python")

;; hy setup
;; (load "lang/hy")

;; haskell setup
(load "lang/haskell")

;; rust setup
;; (load "lang/rust-racer")
(load "lang/rust-lsp")

;; csharp setup
(load "lang/csharp")

;; support for web programming
(load "lang/web")

;; docker setup
(load "lang/docker")

;; after initialization, revert startup preparations
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist last-file-name-handler-alist)

;; restore manual answer prompt
(fset 'y-or-n-p 'old-prompt)
