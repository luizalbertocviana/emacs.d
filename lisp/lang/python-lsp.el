(setq python-indent-offset 4)

(add-hook 'python-mode-hook 'lsp)

(general-define-key
 :states  '(normal)
 :keymaps '(python-mode-map)
 :prefix  "SPC m"
 "b" 'python-shell-send-buffer
 "r" 'run-python
 )
