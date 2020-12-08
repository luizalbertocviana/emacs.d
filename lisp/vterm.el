(use-package vterm)

(general-define-key
  :states  '(normal)
  :keymaps '(override)
  :prefix  "SPC t"
    "T" 'vterm
)
