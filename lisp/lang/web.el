(use-package js2-mode
  :config
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))

(use-package tide
  :hook
    (js2-mode . tide-setup)
    (js2-mode . tide-hl-identifier-mode)
    (before-save . tide-format-before-save))

(use-package skewer-mode
  :config
    (skewer-setup))

(use-package web-mode
  :hook
    (web-mode . lsp)
    (web-mode . emmet-mode)
    (web-mode . skewer-mode)
  :custom
    (web-mode-auto-close-style 1)
    (web-mode-enable-auto-expanding t)
  :init
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  :config
    (sp-local-pair 'web-mode "<" ">" :actions :rem))

(use-package emmet-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'emmet-expand-line)
            (define-key evil-insert-state-local-map (kbd "TAB") 'emmet-expand-line)))

(general-define-key
 :states  '(normal)
 :keymaps '(js2-mode-map)
 :prefix  "SPC m"
 "d"   'tide-jump-to-definition
 "e a" 'tide-project-errors
 "e e" 'tide-error-at-point
 "e f" 'tide-fix
 "f"   'tide-format
 "h"   'tide-documentation-at-point
 "i"   'tide-organize-imports
 "l"   'skewer-load-buffer
 "r R" 'tide-refactor
 "r f" 'tide-rename-file
 "r r" 'tide-references
 "r s" 'tide-rename-symbol
 "s"   'run-skewer
 "z"   'skewer-repl)
