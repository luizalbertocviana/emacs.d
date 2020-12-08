;; key chords
(use-package key-chord
  :custom
    (key-chord-two-keys-delay 0.3)
    (key-chord-one-key-delay 0.3)
  :config
    (key-chord-mode 1)
    ;; jk and kj return to normal state
    (key-chord-define evil-visual-state-map "kj" 'evil-normal-state)
    (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    ;; chords to avoid reaching for shift key
    (key-chord-define evil-insert-state-map "qq" "/")
    (key-chord-define evil-insert-state-map "ww" "?")
    (key-chord-define evil-insert-state-map "''" "\"")
    (key-chord-define evil-insert-state-map "11" "!")
    (key-chord-define evil-insert-state-map "22" "@")
    (key-chord-define evil-insert-state-map "33" "#")
    (key-chord-define evil-insert-state-map "44" "$")
    (key-chord-define evil-insert-state-map "55" "%")
    (key-chord-define evil-insert-state-map "77" "&")
    (key-chord-define evil-insert-state-map "88" "*")
    (key-chord-define evil-insert-state-map "99" "(")
    (key-chord-define evil-insert-state-map "00" ")")
    (key-chord-define evil-insert-state-map "--" "_")
    (key-chord-define evil-insert-state-map "==" "+")
    (key-chord-define evil-insert-state-map "[[" "{"))

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
        "R" 'xref-find-references
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
)
