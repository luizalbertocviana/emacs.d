(use-package elpy
  :init
    (elpy-enable))

(general-define-key
 :states  '(normal)
 :keymaps '(python-mode-map)
 :prefix  "SPC m"
 "b" 'elpy-shell-send-region-or-buffer
 "d" 'elpy-doc
 "e" 'elpy-flymake-next-error
 "f" 'elpy-shell-send-statement
 "r" 'run-python
 "s" 'elpy-rgrep-symbol
 "t" 'elpy-test
)
