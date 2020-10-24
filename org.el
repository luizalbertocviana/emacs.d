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
