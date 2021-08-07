(setq python-indent-offset 4)

(use-package lsp-python-ms
  :custom
    (lsp-python-ms-auto-install-server t)
  :hook
    (python-mode . (lambda ()
                     (require 'lsp-python-ms)
                     (lsp))))

(general-define-key
 :states  '(normal)
 :keymaps '(python-mode-map)
 :prefix  "SPC m"
 "b" 'python-shell-send-buffer
 "r" 'run-python
 )
