(setq python-indent-offset 4)

(use-package lsp-python-ms)

(setq lsp-python-ms-executable (executable-find "python-language-server"))

(add-hook 'python-mode-hook 'lsp)

(general-define-key
 :states  '(normal)
 :keymaps '(python-mode-map)
 :prefix  "SPC m"
 "b" 'python-shell-send-buffer
 "r" 'run-python
 )
