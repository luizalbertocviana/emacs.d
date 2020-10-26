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
    ;; allowed languages
    (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (shell      . t)
        (python     . t)
        (latex      . t)
        (lisp       . t)
        (C          . t)))
    ;; this makes beamer export options avaliable at the org
    ;; dispatcher
    (push 'beamer org-export-backends)
    ;; template used to agenda entries (tags should be at org agenda
    ;; files)
    (setq org-capture-templates
          '(("c" "Capture" entry (file+datetree  "~/Dropbox/org/notes.org")
              "* TODO %? %^g\n  SCHEDULED: %^{Scheduled}t DEADLINE: %^{Deadline}t"))))

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
 "h"   'kill-buffer-and-frame-or-window
 "j"   'org-agenda-next-item
 "k"   'org-agenda-previous-item
 "l"   'org-agenda-show-and-scroll-up
 )
