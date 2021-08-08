(use-package org
  :hook
    ;; provides some completion in org mode
    (org-mode . (lambda () (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)))
  :custom
    ;; suddenly this workaraound has been made needed so
    ;; org-agenda-exit stays working properly. For some reason, this
    ;; variable had been initialized as a string value
    (org-agenda-new-buffers nil)
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

(setq org-agenda-restore-windows-after-quit t)

(add-hook 'org-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map
              (kbd "TAB") 'org-cycle)))

(general-define-key
 :states  '(normal)
 :keymaps '(override)
 :prefix  "SPC o"
 "l" 'org-store-link
 "a" 'org-agenda-list
 "c" 'org-capture
 )

(defhydra org-heading-hydra (org-mode-map nil :columns 4 :exit t)
  "org headings"
  ("h" org-promote-subtree   "promote subtree")
  ("l" org-demote-subtree    "demote subtree")
  ("k" org-move-subtree-up   "move subtree up")
  ("j" org-move-subtree-down "move subtree down")
  ("d" org-cut-subtree       "cut subtree")
  ("y" org-copy-subtree      "copy subtree")
  ("p" org-paste-subtree     "paste subtree")
)

(defhydra org-link-hydra (org-mode-map nil :columns 4 :exit t)
  "org links"
  ("o" org-open-at-point "open")
  ("i" org-insert-link   "insert")
  )

(defhydra org-spreadsheet-column-hydra (org-mode-map nil :columns 4 :exit t)
  "org spreadsheet column"
  ("h" org-table-move-column-left   "move left")
  ("l" org-table-move-column-right  "move right")
  ("d" org-table-move-delete-column "delete")
  ("i" org-table-move-insert-column "insert")
  )

(defhydra org-spreadsheet-row-hydra (org-mode-map nil :columns 4 :exit t)
  "org spreadsheet row"
  ("k" org-table-move-row-up     "move up")
  ("j" org-table-move-row-down   "move down")
  ("d" org-table-move-kill-row   "delete")
  ("i" org-table-move-insert-row "insert")
  ("h" org-table-hline-and-move  "insert hline")
  )

(defhydra org-spreadsheet-hydra (org-mode-map nil :columns 4 :exit t)
  "org spreadsheet"
  ("d" org-table-blank-field                "blank field")
  ("s" org-table-sort-lines                 "sort lines")
  ("u" org-table-iterate-buffer-tables      "update buffer tables")
  ("v" org-table-toggle-coordinate-overlays "toggle row/column labels")
  ("h" org-table-move-cell-left             "move cell left")
  ("j" org-table-move-cell-down             "move cell down")
  ("k" org-table-move-cell-up               "move cell up")
  ("l" org-table-move-cell-right            "move cell right")
  ("c" org-spreadsheet-column-hydra/body    "columns")
  ("r" org-spreadsheet-row-hydra/body       "rows")
  )

(defhydra org-todo-hydra (org-mode-map nil :columns 4 :exit t)
  "org todo"
  ("c" org-todo "change")
  ("d" org-deadline "deadline")
  ("a" org-toggle-archive-tag "toggle archive")
  ("o" org-insert-todo-heading-respect-content "insert")
  ("r" org-clone-subtree-with-time-shift "clone with time shift")
  ("s" org-schedule "schedule")
  ("t" org-show-todo-tree "show todo tree")
  )

(defhydra org-hydra (org-mode-map nil :columns 4 :exit t)
  "org"
  ("D" org-time-stamp               "active timestamp")
  ("d" org-time-stamp-inactive      "inactive timestamp")
  ("S" org-sort                     "sort")
  ("c" org-edit-special             "change")
  ("e" org-export-dispatch          "export")
  ("h" org-heading-hydra/body       "headings")
  ("l" org-link-hydra/body          "links")
  ("r" org-babel-execute-src-block  "run source block")
  ("s" org-spreadsheet-hydra/body   "spreadsheets")
  ("t" org-todo-hydra/body          "todo")
)

(general-define-key :states '(normal)
                    :keymaps 'org-mode-map
                    "SPC m" 'org-hydra/body)

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
