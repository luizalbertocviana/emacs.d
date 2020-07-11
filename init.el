;; startup preparations
(defvar last-file-name-handler-alist file-name-handler-alist)
;; relax garbage collector a little
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      ;; avoid regex search for el and elc files loaded during startup
      file-name-handler-alist nil)

;; we do not use package.el
(setq package-enable-at-startup nil)
;; this keeps emacs from appending variables to the end this file
(setq package--init-file-ensured t)

;; resizing makes no sense if frame is always used maximized
(setq frame-inhibit-implied-resize t)

;; makes the simplest mode possible our initial one
(setq initial-major-mode 'fundamental-mode)

;; inhibit useless and old-school startup screen
(setq inhibit-startup-screen t )
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; substitute "yes or no" prompts for "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; dont use dialog boxes, use minibuffer instead
(setq use-dialog-box nil)

;; confirm before exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; keeps mouse away when typing
(mouse-avoidance-mode 'banish)

;; makes cursor stop blinking
(blink-cursor-mode -1)

;; highlights current line
(global-hl-line-mode 1)

;; indents using spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; breaks lines when they become too long ...
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))
;; ... but this is only applied to comments when in prog mode
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local comment-auto-fill-only-comments t)
            (auto-fill-mode 1)
          ))

;; activates abbrev mode in text mode (useful for correcting typos)
(add-hook 'text-mode-hook 'abbrev-mode)

;; keeps current line from within a margin, regarding both top and
;; bottom lines
(setq scroll-margin 20)

;; font
(add-to-list 'default-frame-alist '(font . "monaco-10"))

;; line numbers setup
;; relative line numbering
(setq display-line-numbers-type 'relative)
;; minimum width (in characters) of line number column
(setq-default display-line-numbers-width 4)
;; activate line number mode globally
(global-display-line-numbers-mode)

;; auto save
(add-hook 'text-mode-hook 'auto-save-mode)
(add-hook 'prog-mode-hook 'auto-save-mode)

;; open files in last edited position
(save-place-mode 1)

;; in prog mode, enable hideshow mode (see keybindings)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; force vertical splittimg, that is, windows are arranged one above
;; another
(setq split-width-threshold nil)

;; makes docview scrolling change document pages
(setq doc-view-continuous t)

;; user information
(setq user-full-name "Luiz Alberto do Carmo Viana")
(setq user-mail-address "luizalbertocviana@gmail.com")

;; the functions below are intended to be used in i3wm keybindings
;; with emacs --eval

;; creates a new eshell buffer
(defun new-eshell ()
  (eshell t))

;; creates dired buffer in current dir
(defun dired-current-dir ()
  (dired "./"))

;; straight package manager initialization
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; bootstrap use-package
(straight-use-package 'use-package)

;; manage matching pairs
(use-package smartparens
  :preface
    (defun lisp-correct-closing-quote ()
      (sp-pair "'" nil :actions :rem)
      (sp-pair "`" nil :actions :rem))
  :config
    ;; the following package is installed automatically
    (use-package smartparens-config :straight nil)
    (smartparens-global-mode t)
    ;; highlights matching pairs
    (show-smartparens-global-mode 1)
    (add-hook 'lisp-mode-hook 'lisp-correct-closing-quote t t)
)

;; spell checking
(use-package flyspell :defer t)

;; colorscheme setup
(use-package doom-themes
  :config
    (load-theme 'doom-one t)
)

;; mode line setup
(setq-default mode-line-format
      (list
       ;; evil state
       '(:eval evil-mode-line-tag)
       ;; directory name
       " "
       '(:eval (abbreviate-file-name default-directory))
       ;; buffer name
       '(:eval (if (buffer-modified-p) " %b* " " %b "))
       ;; spacing to right align the remaining items (no idea how this
       ;; works)
       '(:eval
          (propertize " " 'display
                      `((space :align-to (- (+ right right-fringe right-margin) ,(+ 3 (string-width mode-name)))))
          )
        )
       ;; the following items are right aligned
       ;; major mode
       " %m "
      )
)

;; evil mode setup
(use-package evil
  :init
    (setq evil-search-module 'evil-search)
    ;; uses M-x for emacs commands
    (setq evil-ex-complete-emacs-commands nil)
    ;; like vim's 'splitbelow'
    (setq evil-split-window-below t) 
    (setq evil-shift-round nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-move-beyond-eol t)
    ;; evil everywhere: every mode starts at normal state, and no mode
    ;; keymap overrides evil
    (setq evil-want-integration nil)
    (setq evil-emacs-state-modes nil)
    (setq evil-insert-state-modes nil)
    (setq evil-motion-state-modes nil)
    (setq evil-overriding-maps nil)
    (setq evil-intercept-maps nil)
    ;; state indicators appearence
    (setq evil-normal-state-tag   (propertize " NORMAL "   'face '((:background "olive drab" :foreground "black"))))
    (setq evil-emacs-state-tag    (propertize " EMACS "    'face '((:background "purple"     :foreground "black"))))
    (setq evil-insert-state-tag   (propertize " INSERT "   'face '((:background "orange"     :foreground "black"))))
    (setq evil-motion-state-tag   (propertize " MOTION "   'face '((:background "gray"       :foreground "black"))))
    (setq evil-visual-state-tag   (propertize " VISUAL "   'face '((:background "yellow"     :foreground "black"))))
    (setq evil-operator-state-tag (propertize " OPERATOR " 'face '((:background "gray"       :foreground "black"))))
    ;; cursor appearence
    (setq evil-normal-state-cursor '(box "olive drab"))
    (setq evil-emacs-state-cursor '(box "purple"))
    (setq evil-insert-state-cursor '((bar . 2) "orange"))
    (setq evil-motion-state-cursor '(box "gray"))
    (setq evil-visual-state-cursor '(box "yellow"))
  :config
    (evil-mode)
    ;; undo similar to vim's
    (global-undo-tree-mode)
    ;; ex commands, which a vim user is likely to be familiar with
    (use-package evil-expat :defer t)
    ;; visual editing hints
    (use-package evil-goggles
      :config
        (evil-goggles-mode)
    )
    ;; like vim-surround
    (use-package evil-surround
      :config
        (global-evil-surround-mode 1)
    )
    ;; turns jk into ESC
    (use-package evil-escape
      :custom
        (evil-escape-key-sequence "jk")
        (evil-escape-delay 0.3)
      :config
        (evil-escape-mode)
    )
    ;; assigns to a two text objects concerning comma separated
    ;; arguments
    (use-package evil-args
      :config
        (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
        (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
    )
    ;; this makes M-v selects larger and larger regions in a
    ;; semantic-wise way
    (use-package expand-region
      :config
        (define-key evil-normal-state-map (kbd "M-v") 'er/expand-region)
        (define-key evil-visual-state-map (kbd "M-v") 'er/expand-region)
    )
    ;; some keybindings
    (evil-define-key 'normal 'global
      ;; movements
      "H" "^"
      "J" "}"
      "K" "{"
      "L" "$"
      ;; yanking until end of line
      "Y" "y$"
      ;; macro execution
      "Q" "@q"
    )
    (evil-define-key 'visual 'global
      ;; movements
      "H" "^"
      "J" "}"
      "K" "{"
      "L" "$"
      ;; linewise macro execution
      "Q" (kbd ":norm @q RET")
    )
    ;; makes TAB indent current line or selected text
    (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
    (define-key evil-visual-state-map (kbd "TAB") 'indent-for-tab-command)
    ;; text objects for latex: this shows that defining new text
    ;; objects is not hard at all
    ;; a$
    (evil-define-text-object evil-a-dollar (count &optional beg end type)
      :extend-selection t
      (evil-select-quote ?$ beg end type count t)
    )
    (define-key evil-outer-text-objects-map "$" 'evil-a-dollar)
    ;; i$
    (evil-define-text-object evil-i-dollar (count &optional beg end type)
      :extend-selection nil
      (evil-select-quote ?$ beg end type count nil)
    )
    (define-key evil-inner-text-objects-map "$" 'evil-i-dollar)
)

;; start screen
(use-package dashboard
  :custom
    (dashboard-startup-banner 'logo)
    (dashboard-center-content t)
    (dashboard-set-file-icons nil)
    (dashboard-set-footer     nil)
    (dashboard-items          '((projects . 10)))
    (initial-buffer-choice    (lambda () (get-buffer "*dashboard*")))
  :config
    (dashboard-setup-startup-hook)
)

;; colorful delimiters (useful for editing lisp languages)
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook
    (emacs-lisp-mode . rainbow-delimiters-mode)
    (lisp-mode       . rainbow-delimiters-mode)
    (clojure-mode    . rainbow-delimiters-mode)
    (hy-mode         . rainbow-delimiters-mode)
)

;; ivy, providing some completion facilities to certain emacs contexts
(use-package ivy
  :custom
    (ivy-use-virtual-buffers t)
    (enable-recursive-minibuffers t)
  :config
    (ivy-mode 1)
    (use-package counsel
      :config
        (counsel-mode 1)
    )
    (use-package ivy-posframe
      :custom
        (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
      :config
        (ivy-posframe-mode 1)
    )
)

;; projectile
(use-package projectile
  :custom
    (projectile-completion-system 'ivy)
  :config
    (projectile-mode +1)
)

;; dired file manager
(use-package dired-x :straight nil
  :preface
    (defun dired-open-file ()
      "In dired, open the file named on this line."
      (interactive)
      (let* ((file (dired-get-filename nil t)))
        (call-process "xdg-open" nil 0 nil file)))
  :hook
    (dired-mode . dired-omit-mode)
  :custom
    ;; do what i mean, makes operations intuitive when there are two
    ;; open dired buffers
    (dired-dwim-target t)  
    ;; dired does not change last modified timestamp when copying
    ;; files
    (dired-copy-preserve-time t)
    ;; human readable sizes
    (dired-listing-switches "-alh")
  :config
    ;; this command is disabled by default, but I like this better
    ;; than 'dired-find-file
    (put 'dired-find-alternate-file 'disabled nil)
    ;; this prevents dot files from being listed (this cannot be put
    ;; into a :custom section (dont know why))
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
    ;; this makes dired list directories first
    (use-package ls-lisp :straight nil
      :custom
        (ls-lisp-dirs-first t)
        (ls-lisp-use-insert-directory-program nil)
    )
)

;; displays keybindings
(use-package which-key
  :config
    (which-key-mode)
)

;; autocompletion
(use-package company
  :hook
    (after-init . global-company-mode)
  :custom
    (company-idle-delay 0)
  :config
    ;; displays a help popup window
    (use-package company-quickhelp
      :config
        (company-quickhelp-mode)
    )
    ;; LaTeX stuff
    (use-package company-auctex
      :commands (company-auctex-init)
    )
    (use-package company-math
      :after (company-auctex)
      :config
        (push 'company-math-symbols-latex company-backends)
        (push 'company-latex-commands     company-backends)
    )
    (use-package company-reftex
      :after (company-auctex)
      :custom
        (company-reftex-max-annotation-length 80)
    )
)

;; org
(use-package org
  :straight org-plus-contrib
  :hook
    ;; provides some completion in org mode
    (org-mode . (lambda () (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)))
  :custom
    ;; runs code snippets with no confirmation prompt
    (org-confirm-babel-evaluate nil)
    (org-log-done t)
    (org-default-notes-file "~/Dropbox/org/notes.org")
    (org-agenda-files (list "~/Dropbox/org/notes.org"))
    ;; shows org agenda for the current month
    (org-agenda-span 'month)
    ;; aligns tags on a reasonable column
    (org-agenda-tags-column 80)
    ;; LaTeX exporting options
    (org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                             "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    ;; uses sly to evaluate lisp code
    (org-babel-lisp-eval-fn 'sly-eval)
    ;; accepts letters as bullet points for lists
    (org-list-allow-alphabetical t)
  :config
    ;; add package minted (with options) to auto generated tex files
    (push '("newfloat, cache = false" "minted") org-latex-packages-alist)
    ;; allowed languages
    (org-babel-do-load-languages
      'org-babel-load-languages
      '(
        (emacs-lisp . t)
        (shell      . t)
        (python     . t)
        (latex      . t)
        (lisp       . t)
        (C          . t)
       )
    )
    ;; this makes beamer export options avaliable at the org
    ;; dispatcher
    (push 'beamer org-export-backends)
    ;; template used to agenda entries (tags should be at org agenda
    ;; files)
    (setq org-capture-templates
          '(
             ("c" "Capture" entry (file+datetree  "~/Dropbox/org/notes.org")
              "* TODO %? %^g\n  SCHEDULED: %^{Scheduled}t DEADLINE: %^{Deadline}t"
             )
           )
    )
)

;; mail/news reader
(use-package gnus :straight nil
  :defer t
  :custom
    (gnus-auto-center-summary nil)
    (gnus-blocked-images      "ads")
    (gnus-ignored-newsgroups  "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
    (gnus-novice-user         t)
    (gnus-show-all-headers    nil)
  :hook
    (gnus-select-group . gnus-group-set-timestamp)
  :config
    (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
    ;; setting gmail
    (setq smtpmail-smtp-server  "smtp.gmail.com")
    (setq smtpmail-smtp-service 587)
    (setq gnus-select-method
          '(nnimap "gmail"
                   (nnimap-address "imap.gmail.com")
                   (nnimap-server-port "imaps")
                   (nnimap-stream ssl)))
)

;; magit
(use-package magit
  :commands (magit-status magit-dispatch)
)

;; vterm
(use-package vterm)

;; LaTeX
(use-package tex-site
  :straight auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook
    ;; synctex and stuff
    (LaTeX-mode . TeX-source-correlate-mode)
    ;; prepares to pretty-print some symbols
    (LaTeX-mode . TeX-fold-mode)
  :custom
    (TeX-auto-save t)
    (TeX-parse-self t)
    (TeX-master nil)
    (TeX-source-correlate-method 'synctex)
    (TeX-source-correlate-start-server t)
  :config
    (defcustom LaTeX-inhibited-auto-fill-environments
      '("tabular" "tikzpicture")
      "latex environments where auto-fill is disabled"
    )
    (defun LaTeX-inhibited-auto-fill-environment-p (environment)
      (member environment LaTeX-inhibited-auto-fill-environments)
    )
    ;; only auto-fills when outside of inhibited environments
    (defun LaTeX-limited-auto-fill ()
      (let ((environment (LaTeX-current-environment)))
        (unless (LaTeX-inhibited-auto-fill-environment-p environment)
          (do-auto-fill))))
    (add-hook 'LaTeX-mode-hook (lambda () (setq-local auto-fill-function 'LaTeX-limited-auto-fill)))
    ;; pretty-prints some symbols and commands when entering and
    ;; saving latex files
    (add-hook 'LaTeX-mode-hook 'TeX-fold-buffer t)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (add-hook 'after-save-hook 'TeX-fold-buffer t t)
              ))
    ;; general setup: reftex, company, pdf mode and reftex isearch
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (reftex-mode)
                (company-auctex-init)
                (setq-local company-backends
                            (append '((company-reftex-labels company-reftex-citations))
                                    company-backends))
                (TeX-PDF-mode t)
                (reftex-isearch-minor-mode)
              ))
    ;; use zathura as a pdf viewer
    (with-eval-after-load 'tex
      (add-to-list 'TeX-view-program-selection
                   '(output-pdf "Zathura")))
    ;; adds some facilities like folding
    (use-package latex-extra
      :hook
        (LaTeX-mode . latex-extra-mode)
      :config
        (define-key evil-normal-state-local-map (kbd "TAB") 'latex/hide-show)
    )
)

;; LaTeX references and citations
(use-package reftex
  :commands (reftex-mode)
  :custom
    (reftex-plug-into-AUCTeX t)
)

;; bibtex searching
(use-package biblio
  :commands (biblio-lookup)
)

;; lisp sly
(use-package sly
  :hook
    (lisp-mode . sly-mode)
  :custom
    (inferior-lisp-program "sbcl")
)

;; deals with language server protocol
(use-package eglot
  :hook
    (c++-mode    . eglot-ensure)
    (python-mode . eglot-ensure)
  :config
    ;; python
    (add-to-list 'eglot-server-programs
                 `(python-mode . ("python" "-m" "pyls")))
)

;; gdb setup
(setq gdb-display-io-nopopup t)

;; python setup
(setq python-indent-offset 4)
;; jupyter setup
(use-package jupyter)

;; hy mode
(use-package hy-mode)

;; keybindings
(use-package general
  :config
    ;; naming some prefixes
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
      :prefix  "SPC"
        "TAB" '(:ignore t :which-key "last buffer")
        "SPC" '(:ignore t :which-key "more")
        "S"   '(:ignore t :which-key "spell")
        "T"   '(:ignore t :which-key "text")
        "b"   '(:ignore t :which-key "buffers")
        "f"   '(:ignore t :which-key "files")
        "h"   '(:ignore t :which-key "help")
        "m"   '(:ignore t :which-key "mode")
        "o"   '(:ignore t :which-key "org")
        "p"   '(:ignore t :which-key "program")
        "s"   '(:ignore t :which-key "search")
        "t"   '(:ignore t :which-key "terminal")
        "w"   '(:ignore t :which-key "windows")
        "v"   '(:ignore t :which-key "version control")
    )
    ;; some simple actions
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
      :prefix  "SPC"
        "TAB" (lambda () (interactive) (switch-to-buffer (other-buffer)))
        "SPC" 'tmm-menubar
        "P"   'list-processes
        "a"   'counsel-linux-app
        "c"   'calendar
        "g"   'gnus
        "i"   'imenu
        "q"   'delete-frame
        "r"   'async-shell-command
    )
    ;; terminal menu
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
      :prefix  "SPC t"
        "t" 'eshell
        "T" 'vterm
    )
    ;; help menu
    (general-define-key
      :states '(normal)
      :leymaps '(override)
      :prefix "SPC h"
        "f" 'describe-function
        "h" 'help-for-help
        "k" 'describe-key
        "m" 'describe-mode
        "v" 'describe-variable
    )
    ;; buffer menu
    (general-define-key
     :states  '(normal)
     :keymaps '(override)
     :prefix  "SPC b"
       "D" 'kill-some-buffers
       "B" 'buffer-menu
       "b" 'ivy-switch-buffer
       "d" 'kill-this-buffer
       "e" 'eval-buffer
       "q" 'kill-buffer-and-window
       "y" 'clone-indirect-buffer-other-window
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(Buffer-menu-mode-map)
        "U" 'Buffer-menu-unmark-all
        "l" 'Buffer-menu-this-window
        "o" 'Buffer-menu-other-window
        "s" 'Buffer-menu-save
        "u" 'Buffer-menu-unmark
        "x" 'Buffer-menu-execute
        "d" 'Buffer-menu-delete
        "h" 'quit-window
    )
    ;; file management
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
      :prefix  "SPC f"
        "D"   'diff-buffer-with-file
        "R"   'recover-this-file
        "S"   'write-file
        "U"   'revert-buffer
        "m"   'counsel-bookmark
        "o"   '(:ignore t :which-key "dired")
        "o"   (lambda () (interactive) (dired "./"))
        "p c" 'projectile-commander
        "p g" 'projectile-grep
        "p o" 'projectile-find-file-dwim
        "p p" 'projectile-switch-project
        "p r" 'projectile-replace-regexp
        "p"   '(:ignore t :which-key "project")
        "r"   'counsel-recentf
        "s"   'save-buffer
    )
    ;; spell checking
    (general-define-key
      :states  '(normal visual)
      :keymaps '(override)
      :prefix  "SPC S"
        "a" 'inverse-add-global-abbrev
        "c" 'ispell-change-dictionary
        "p" 'flyspell-prog-mode
        "e" '(:ignore t :which-key "enable")
        "e" (lambda ()
              (interactive)
              (flyspell-mode)
              (flyspell-buffer))
        "d" '(:ignore t :which-key "disable")
        "d" (lambda ()
              (interactive)
              (flyspell-mode-off))
        "w" 'ispell-word
        "s" 'flyspell-auto-correct-word
    )
    ;; window management and navigation
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
      :prefix  "SPC w"
        "w" 'other-window
        "h" 'windmove-left
        "j" 'windmove-down
        "k" 'windmove-up
        "l" 'windmove-right
        "d" 'delete-window
        "m" 'delete-other-windows
        "s" 'split-window-below
        "v" 'split-window-right
    )
    ;; version control
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
      :prefix  "SPC v"
        "G"   'magit-dispatch
        "P"   'vc-push
        "V"   'vc-annotate
        "c"   'vc-resolve-conflicts
        "d"   'vc-diff
        "g"   'magit-status
        "p"   'vc-pull
        "r"   'vc-revision-other-window
        "u"   'vc-revert
        "v"   'vc-next-action
        "o"   '(:ignore t :which-key "vc-dir")
        "o"   (lambda () (interactive) (vc-dir "./"))
        "l"   '(:ignore t :which-key "log")
        "l i" 'vc-log-incoming
        "l l" 'vc-print-log
        "l o" 'vc-log-outgoing
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(vc-dir-mode-map)
        "J" 'vc-dir-next-directory
        "K" 'vc-dir-previous-directory
        "M" 'vc-dir-unmark
        "O" 'vc-dir-find-file-other-window
        "h" 'vc-dir-hide-up-to-date
        "j" 'vc-dir-next-line
        "k" 'vc-dir-previous-line
        "m" 'vc-dir-mark
        "o" 'vc-dir-find-file
        "s" (lambda () (interactive) (vc-dir-isearch-regexp))
        "v" 'vc-next-action
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(vc-dir-mode-map)
      :prefix  "SPC m"
        "B" 'vc-create-tag
        "L" 'vc-print-branch-log
        "b" 'vc-retrieve-tag
        "d" 'vc-root-diff
        "i" 'vc-ignore
        "l" 'vc-print-root-log
        "m" 'vc-merge
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(log-edit-mode-map)
      :prefix  "SPC m"
        "d" 'log-edit-show-diff
        "f" 'log-edit-show-files
    )
    (general-define-key
      :states  '(normal visual)
      :keymaps '(log-view-mode-map)
        "D" 'log-view-diff-changeset
        "a" 'log-view-annotate-version
        "d" 'log-view-diff
        "h" 'log-view-file-prev
        "j" 'log-view-msg-next
        "k" 'log-view-msg-prev
        "l" 'log-view-file-next
        "o" 'log-view-find-revision
    )
    ;; some search facilities
    (general-define-key
      :states  '(normal visual)
      :keymaps '(override)
      :prefix  "SPC s"
        "s" 'isearch-forward-symbol-at-point
        "r" 'query-replace
        "R" 'replace-string
    )
    (general-define-key
      :keymaps '(isearch-mode-map)
        "n" 'isearch-repeat-forward
        "N" 'isearch-repeat-backward
    )
    ;; some text manipulations
    (general-define-key
      :states  '(normal visual)
      :keymaps '(override)
      :prefix  "SPC T"
        "A"     'align-regexp
        "S"     'sort-fields
        "a"     'align
        "c"     'capitalize-dwim
        "f"     'fill-paragraph
        "l"     'downcase-dwim
        "r"     'reverse-region
        "s"     'sort-lines
        "t"     '(:ignore t :which-key "table")
        "t C"   'table-capture
        "t R"   'table-recognize
        "t S"   'table-generate-source
        "t a"   'table-justify
        "t c"   '(:ignore t :which-key "columns")
        "t c d" 'table-delete-column
        "t c i" 'table-insert-column
        "t i"   'table-insert
        "t m"   'table-span-cell
        "t r"   '(:ignore t :which-key "rows")
        "t r d" 'table-delete-row
        "t r i" 'table-insert-row
        "t s"   'table-split-cell
        "u"     'upcase-dwim
    )
    ;; programming facilities
    (general-define-key
      :states  '(normal visual)
      :keymaps '(override)
      :prefix  "SPC p"
        "b" 'compile
        "c" 'comment-line
        "d" 'xref-find-definitions
        "e" 'next-error
        "f" 'mark-defun
        "p" 'check-parens
        "r" 'recompile
        ;; these only work in prog mode (or if you enable hs-minor-mode)
        "H" 'hs-hide-all
        "h" 'hs-toggle-hiding
    )
    ;; cursor character-wise movements in insert mode
    (general-define-key
      :states  '(insert)
      :keymaps '(override)
        "C-h" 'backward-char
        "C-l" 'forward-char
        "C-j" 'next-line
        "C-k" 'previous-line
    )
    ;; cursor sexp-wise movements in normal mode
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
        "C-h" 'backward-sexp
        ;; C-l puts cursor at the beginning of next sexp; forward-sexp
        ;; alone would put cursor at the end of next sexp
        "C-l" (lambda () (interactive) (forward-sexp 2) (backward-sexp))
        "C-j" 'down-list
        "C-k" 'backward-up-list
    )
    ;; cursor movements in normal mode
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
        "M-l" 'forward-sentence
        "M-h" 'backward-sentence
    )
    ;; dashboard
    (general-define-key
      :states  '(normal)
      :keymaps 'dashboard-mode-map
        "p" (general-simulate-key "p" :state 'emacs)
        "l" 'dashboard-return
    )
    ;; ivy minibuffer
    (general-define-key
      :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
        "C-h" 'keyboard-escape-quit
        "C-j" 'ivy-next-line
        "C-k" 'ivy-previous-line
        "C-l" 'ivy-alt-done
    )
    ;; dired
    (general-define-key
      :states  '(normal)
      :keymaps 'dired-mode-map
        "J" 'dired-next-marked-file
        "K" 'dired-prev-marked-file
        "M" 'dired-unmark
        "U" 'dired-unmark-all-marks
        "h" (lambda () (interactive) (find-alternate-file ".."))
        "j" 'dired-next-line
        "k" 'dired-previous-line
        "l" 'dired-find-alternate-file
        "m" 'dired-mark
        "n" 'evil-ex-search-next
        "o" 'dired-open-file
        "p" 'evil-ex-search-previous
        "s" 'evil-ex-search-forward
        "u" 'dired-undo
        "y" 'dired-copy-filename-as-kill
    )
    (general-define-key
      :states  '(normal)
      :keymaps 'dired-mode-map
      :prefix  "SPC m"
        "D" 'dired-diff
        "S" 'dired-do-symlink
        "T" 'dired-toggle-marks
        "U" 'dired-upcase
        "Z" 'dired-do-compress
        "c" 'wdired-change-to-wdired-mode
        "d" 'dired-do-delete
        "g" 'dired-do-chgrp
        "h" 'dired-omit-mode
        "i" (lambda () (interactive) (image-dired "./"))
        "l" 'dired-downcase
        "m" 'dired-do-chmod
        "n" 'dired-create-directory
        "o" 'dired-do-chown
        "p" 'dired-do-print
        "r" 'dired-do-rename
        "s" 'dired-do-isearch-regexp
        "t" 'dired-do-async-shell-command
        "y" 'dired-do-copy
        "z" 'dired-do-compress-to
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(image-dired-thumbnail-mode-map)
        "h" 'image-dired-backward-image
        "l" 'image-dired-forward-image
        "j" 'image-dired-next-line
        "k" 'image-dired-previous-line
    )
    ;; archive mode
    (general-define-key
      :states  '(normal)
      :keymaps 'archive-mode-map
        "j" 'archive-next-line
        "k" 'archive-previous-line
        "l" 'archive-extract
        "m" 'archive-flag-deleted
    )
    (general-define-key
      :states  '(normal)
      :keymaps 'archive-mode-map
      :prefix  "SPC m"
        "g" 'archive-chgrp-entry
        "m" 'archive-chmod-entry
        "o" 'archive-chown-entry
        "d" 'archive-expunge
        "r" 'archive-rename-entry
    )
    ;; org
    (general-define-key
      :states  '(normal)
      :keymaps '(override)
      :prefix  "SPC o"
        "l" 'org-store-link
        "a" 'org-agenda-list
        "c" 'org-capture
    )
    (general-define-key
      :states  '(normal)
      :keymaps 'org-mode-map
      :prefix  "SPC m"
        ;; timestamps
        "D"     'org-time-stamp
        "d"     'org-time-stamp-inactive
        ;; sorting
        "S"     'org-sort
        ;; editing
        "c"     'org-edit-special
        ;; exporting
        "e"   'org-export-dispatch
        ;; headings
        "h h"   'org-promote-subtree
        "h l"   'org-demote-subtree
        "h k"   'org-move-subtree-up
        "h j"   'org-move-subtree-down
        "h d"   'org-cut-subtree
        "h y"   'org-copy-subtree
        "h p"   'org-paste-subtree
        ;; links
        "l o"   'org-open-at-point
        "l l"   'org-insert-link
        ;; run source block
        "r"     'org-babel-execute-src-block
        ;; spreadsheet
        "s d"   'org-table-blank-field
        "s s"   'org-table-sort-lines
        "s u"   'org-table-iterate-buffer-tables
        "s v"   'org-table-toggle-coordinate-overlays
        "s h"   'org-table-move-cell-left
        "s j"   'org-table-move-cell-down
        "s k"   'org-table-move-cell-up
        "s l"   'org-table-move-cell-right
        "s c h" 'org-table-move-column-left
        "s c l" 'org-table-move-column-right
        "s c d" 'org-table-delete-column
        "s c i" 'org-table-insert-column
        "s r k" 'org-table-move-row-up
        "s r j" 'org-table-move-row-down
        "s r d" 'org-table-kill-row
        "s r o" 'org-table-insert-row
        "s r h" 'org-table-hline-and-move
        ;; todo
        "t c"   'org-todo
        "t d"   'org-deadline
        "t a"   'org-toggle-archive-tag
        "t o"   'org-insert-todo-heading-respect-content
        "t r"   'org-clone-subtree-with-time-shift
        "t s"   'org-schedule
        "t v"   'org-show-todo-tree
    )
    (general-define-key
      :keymaps 'org-read-date-minibuffer-local-map
        "h" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1)))
        "l" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))
        "j" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1)))
        "k" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1)))
        "H" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1)))
        "L" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1)))
        "J" (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1)))
        "K" (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1)))
    )
    (general-define-key
      :states  '(normal)
      :keymaps 'org-agenda-mode-map
        "C-j" 'org-agenda-do-date-later
        "C-k" 'org-agenda-do-date-earlier
        "J"   'org-agenda-later
        "K"   'org-agenda-earlier
        "M"   'org-agenda-bulk-action
        "c"   'org-agenda-date-prompt
        "d"   'org-agenda-kill
        "m"   'org-agenda-bulk-toggle
        "r"   'org-agenda-redo
        "t"   'org-agenda-todo
        "u"   'org-agenda-undo
        "h"   'org-agenda-exit
        "j"   'org-agenda-next-item
        "k"   'org-agenda-previous-item
        "l"   'org-agenda-show-and-scroll-up
    )
    ;; calendar mode
    (general-define-key
      :states  '(normal)
      :keymaps '(calendar-mode-map)
        "." 'calendar-goto-today
        "H" 'calendar-backward-month
        "J" 'calendar-forward-year
        "K" 'calendar-backward-year
        "L" 'calendar-forward-month
        "g" 'calendar-goto-date
        "h" 'calendar-backward-day
        "j" 'calendar-forward-week
        "k" 'calendar-backward-week
        "l" 'calendar-forward-day
        "q" 'calendar-exit
    )
    ;; image mode
    (general-define-key
      :states  '(normal)
      :keymaps '(image-mode-map)
        "h" 'image-previous-file
        "l" 'image-next-file
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(image-mode-map)
      :prefix  "SPC m"
        "g" 'image-toggle-animation
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(doc-view-mode-map)
        "+"  'doc-view-enlarge
        "-"  'doc-view-shrink
        "G"  'doc-view-last-page
        "J"  'doc-view-next-page
        "K"  'doc-view-previous-page
        "S"  'doc-view-search-backward
        "gg" 'doc-view-first-page
        "j"  'doc-view-next-line-or-next-page
        "k"  'doc-view-previous-line-or-previous-page
        "s"  'doc-view-search
    )
    ;; process mode
    (general-define-key
      :states  '(normal)
      :keymaps '(process-menu-mode-map)
      :prefix "SPC m"
      "d" 'process-menu-delete-process
    )
    ;; company completion popup.  I am using Meta because Control is
    ;; already bounded
    (general-define-key
      :states  '(insert)
      :keymaps '(company-active-map override)
        "M-j" 'company-select-next
        "M-k" 'company-select-previous
        "M-l" 'company-complete
    )
    ;; magit mode
    (general-define-key
      :states '(normal)
      :keymaps 'magit-mode-map
        "RET" 'magit-visit-thing
        "TAB" 'magit-section-toggle
        "P"   'magit-push
        "a"   'magit-commit-amend
        "c"   'magit-commit
        "i"   'magit-gitignore
        "j"   'magit-next-line
        "k"   'magit-previous-line
        "q"   'magit-mode-bury-buffer
        "r"   'magit-refresh
        "s"   'magit-stage
        "u"   'magit-unstage
    )
    ;; latex mode
    (general-define-key
      :states  '(normal visual)
      :keymaps 'LaTeX-mode-map
      :prefix  "SPC m"
        "B" 'TeX-command-master
        "b" 'TeX-command-run-all
        "e" 'LaTeX-environment
        "l" 'reftex-label
        "m" 'TeX-insert-macro
        "p" 'preview-buffer
        "s" 'LaTeX-section
        "v" 'TeX-view
    )
    ;; bibtex mode
    (general-define-key
      :states  '(normal)
      :keymaps 'bibtex-mode-map
      :prefix  "SPC m"
        "s" 'biblio-lookup
    )
    ;; biblio selection mode
    (general-define-key
      :states  '(normal)
      :keymaps 'biblio-selection-mode-map
        "i" 'biblio--selection-insert
        "y" 'biblio--selection-copy-quit
        "h" 'biblio-kill-buffers
    )
    ;; python mode
    (general-define-key
      :states  '(normal)
      :keymaps '(python-mode-map)
      :prefix  "SPC m"
        "b" 'jupyter-eval-buffer
        "d" 'jupyter-inspect-at-point
        "f" 'jupyter-eval-defun
        "r" 'jupyter-run-repl
    )
    ;; c++ mode
    (general-define-key
      :states  '(normal)
      :keymaps '(c++-mode-map)
      :prefix  "SPC m"
        "b" 'compile
        "d" 'gdb
        "e" 'next-error
        "r" 'recompile
    )
    ;; gdb mode
    (general-define-key
      :states  '(normal)
      :keymaps '(gud-mode-map)
      :prefix "SPC m"
        "b" 'gdb-frame-breakpoints-buffer
        "d" 'gdb-frame-disassembly-buffer
        "i" 'gdb-frame-io-buffer
        "l" 'gdb-frame-locals-buffer
        "m" 'gdb-frame-memory-buffer
        "s" 'gdb-frame-stack-buffer
    )
    ;; lisp mode
    (general-define-key
      :states  '(normal)
      :keymaps '(lisp-mode-map sly-mrepl-mode-map)
      :prefix  "SPC m"
        "A" 'sly-disassemble-symbol
        "C" 'sly-calls-who
        "D" 'sly-edit-uses
        "S" 'sly-stickers-replay
        "T" 'sly-trace-dialog
        "a" 'sly-apropos-all
        "b" 'sly-compile-and-load-file
        "c" 'sly-who-calls
        "d" 'sly-edit-definition
        "e" 'sly-expand-1
        "f" 'sly-compile-defun
        "h" 'sly-describe-symbol
        "l" 'sly-load-file
        "m" 'sly-who-macroexpands
        "r" 'sly
        "s" 'sly-stickers-dwim
        "t" 'sly-trace-dialog-toggle-trace
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(sly-mrepl-mode-map)
        "M-j" 'sly-mrepl-next-prompt
        "M-k" 'sly-mrepl-previous-prompt
    )
    (general-define-key
      :states  '(insert)
      :keymaps '(sly-mrepl-mode-map)
        "<down>" 'sly-mrepl-next-input-or-button
        "<up>"   'sly-mrepl-previous-input-or-button
        "RET"    'sly-mrepl-return
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(sly-stickers--replay-mode-map)
        "n" 'sly-stickers-replay-next
        "p" 'sly-stickers-replay-prev
        "q" 'kill-buffer-and-window
    )
    ;; clojure mode
    (general-define-key
      :states  '(normal)
      :keymaps '(clojure-mode-map)
      :prefix "SPC m"
        "L" 'cider-load-all-files
        "b" 'cider-load-buffer
        "d" 'cider-debug-defun-at-point
        "e" 'cider-macroexpand-1
        "f" 'cider-eval-defun-at-point
        "h" 'cider-doc
        "i" 'cider-inspect
        "l" 'cider-load-file
        "r" 'cider-jack-in
    )
    ;; gnus
    (general-define-key
      :states  '(normal)
      :keymaps '(gnus-group-mode-map)
        "J" 'gnus-group-next-unread-group
        "K" 'gnus-group-prev-unread-group
        "M" 'gnus-group-unmark-group
        "U" 'gnus-group-unmark-all-groups
        "l" 'gnus-topic-select-group
        "m" 'gnus-group-mark-group
        "q" 'gnus-group-exit
        "r" 'gnus-group-get-new-news
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(gnus-group-mode-map)
      :prefix  "SPC m"
        "L"   'gnus-group-list-all-groups
        "R"   'gnus-group-catchup-current-all
        "S"   'gnus-group-enter-server-mode
        "d"   'gnus-topic-kill-group
        "k"   'gnus-group-list-killed
        "l"   'gnus-group-list-groups
        "s"   'gnus-group-unsubscribe-current-group
        "y"   'gnus-topic-yank-group
        "z"   'gnus-group-list-zombies
        "m"   'gnus-group-mail
        "t d" 'gnus-topic-delete
        "t h" 'gnus-topic-unindent
        "t l" 'gnus-topic-indent
        "t m" 'gnus-topic-move-group
        "t n" 'gnus-topic-create-topic
        "t r" 'gnus-topic-rename
        "t s" 'gnus-topic-sort-groups-by-alphabet
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(gnus-summary-mode-map)
        "J" 'gnus-summary-next-unread-subject
        "K" 'gnus-summary-prev-unread-subject
        "Q" 'gnus-summary-exit-no-update
        "h" (lambda () (interactive) (gnus-summary-prev-page 1 nil))
        "l" (lambda () (interactive) (gnus-summary-next-page 1 nil t))
        "q" 'gnus-summary-exit
        "r" 'gnus-summary-prepare
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(gnus-summary-mode-map)
      :prefix  "SPC m"
        "C"   'gnus-summary-cancel-article
        "D"   'gnus-summary-enter-digest-group
        "R"   'gnus-summary-very-wide-reply-with-original
        "a"   'gnus-article-save-part
        "b"   'gnus-summary-set-bookmark
        "d"   'gnus-summary-delete-article
        "f"   'gnus-summary-mail-forward
        "m R" 'gnus-summary-catchup
        "m U" 'gnus-summary-clear-mark-forward
        "m d" 'gnus-summary-mark-as-dormant
        "m r" 'gnus-summary-mark-as-read-forward
        "m u" 'gnus-summary-tick-article
        "p"   'gnus-summary-print-article
        "r"   'gnus-summary-reply-with-original
        "s m" 'gnus-summary-mail-other-window
        "s n" 'gnus-summary-news-other-window
        "v A" 'gnus-summary-limit-to-address
        "v a" 'gnus-summary-limit-to-author
        "v b" 'gnus-summary-limit-to-bodies
        "v r" 'gnus-summary-limit-to-unread
        "v s" 'gnus-summary-limit-to-subject
        "v t" 'gnus-summary-limit-to-age
        "w"   'gnus-summary-save-article-file
        "W"   'gnus-summary-write-article-file
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(gnus-server-mode-map)
        "R" 'gnus-server-regenerate-server
        "c" 'gnus-server-edit-server
        "d" 'gnus-server-kill-server
        "o" 'gnus-server-read-server
        "p" 'gnus-server-yank-server
        "q" 'gnus-server-exit
        "r" 'gnus-server-scan-server
        "v" 'gnus-server-show-server
        "y" 'gnus-server-copy-server
    )
    (general-define-key
      :states  '(normal)
      :keymaps '(gnus-server-mode-map)
      :prefix  "SPC m"
        "C" 'gnus-server-close-all-servers
        "O" 'gnus-server-open-all-servers
        "c" 'gnus-server-close-server
        "d" 'gnus-server-deny-server
        "l" 'gnus-server-list-servers
        "n" 'gnus-server-add-server
        "o" 'gnus-server-open-server
        "u" 'gnus-server-remove-denials
    )
)

;; after initialization, revert startup preparations
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist last-file-name-handler-alist)
