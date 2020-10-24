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

;; custom functions
(load "functions")

;; start straight package manager
(load "init-straight")

;; commonly used packages
(load "common")

;; modeline setup
(load "modeline")

;; evil setup
(load "evil-setup")

;; LaTeX setup
(load "latex")

;; org setup
(load "org")

;; common lisp setup
(load "common-lisp")

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

;; keybindings setup
(load "keybindings")

;; keybindings
(use-package general
  :preface
    (defun kill-buffer-and-frame-or-window ()
      "kills current buffer and its window. When its window is the
only one in the current frame, kill the frame instead"
      (interactive)
      (when (kill-buffer)
        (if (one-window-p)
            (delete-frame)
          (delete-window))))
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
       "q" 'kill-buffer-and-frame-or-window
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
        "V"     'magit-dispatch
        "c"     '(:ignore t :which-key "conflict")
        "c N"   'smerge-prev
        "c R"   'smerge-resolve-all
        "c k"   '(:ignore t :which-key "keep")
        "c k a" 'smerge-keep-all
        "c k b" 'smerge-keep-base
        "c k c" 'smerge-keep-current
        "c k l" 'smerge-keep-lower
        "c k u" 'smerge-keep-upper
        "c n"   'smerge-next
        "c r"   'smerge-resolve
        "c s"   'smerge-swap
        "g"     'magit-status
        "v"     'magit-file-dispatch
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
        "N" 'evil-ex-search-previous
        "U" 'dired-unmark-all-marks
        "h" (lambda () (interactive) (find-alternate-file ".."))
        "j" 'dired-next-line
        "k" 'dired-previous-line
        "l" 'dired-find-alternate-file
        "m" 'dired-mark
        "n" 'evil-ex-search-next
        "o" 'browse-url-of-dired-file
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
        "b"   'magit-branch
        "c"   'magit-commit
        "f"   'magit-fetch
        "i"   'magit-gitignore
        "j"   'magit-next-line
        "k"   'magit-previous-line
        "p"   'magit-pull
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
    ;; hy mode
    (general-define-key
      :states  '(normal)
      :keymaps '(hy-mode-map)
      :prefix  "SPC m"
        "b" 'hy-shell-eval-buffer
        "d" 'hy-describe-thing-at-point
        "e" 'hy-shell-eval-current-form
        "i" 'hy-jedhy-update-imports
        "r" 'run-hy
    )
    ;; rustic mode
    (general-define-key
      :states  '(normal)
      :keymaps '(rustic-mode-map)
      :prefix  "SPC m"
        "C" 'rustic-cargo-clippy
        "D a" 'rustic-cargo-add
        "D r" 'rustic-cargo-rm
        "D u" 'rustic-cargo-upgrade
        "b" 'rustic-cargo-run
        "c" 'rustic-cargo-check
        "d" 'xref-find-definitions
        "e" 'next-error
        "f" 'rustic-format-buffer
        "m" 'rustic-popup
        "r" 'rustic-recompile
        "t" 'rustic-cargo-test
    )
    ;; haskell mode
    (general-define-key
      :states  '(normal)
      :keymaps '(haskell-mode-map)
      :prefix  "SPC m"
        "C" 'haskell-cabal-visit-file
        "I" 'haskell-mode-format-imports
        "b" 'haskell-process-cabal-build
        "c" 'haskell-process-cabal
        "i" 'haskell-process-do-info
        "l" 'haskell-process-load-file
        "r" 'haskell-interactive-switch
        "t" 'haskell-process-do-type
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

;; restore manual answer prompt
(fset 'y-or-n-p 'old-prompt)
