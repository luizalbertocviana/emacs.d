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

(use-package emmet-mode
  :hook
    (sgml-mode . emmet-mode)
    (css-mode . emmet-mode))
