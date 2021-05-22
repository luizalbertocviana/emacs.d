(use-package js2-mode
  :config
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package tide
  :hook
    (js2-mode . tide-setup)
    (js2-mode . tide-hl-identifier-mode))

(use-package skewer-mode
  :config
    (skewer-setup))

(use-package emmet-mode)

(use-package web-mode
  :hook
    (web-mode . lsp)
    (web-mode . emmet-mode)
    (web-mode . skewer-mode)
  :custom
    (web-mode-auto-close-style 2)
    (web-mode-enable-auto-expanding t)
  :init
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  :config
    (sp-local-pair 'web-mode "<" ">" :actions :rem))
