(setq python-indent-offset 4)

;; lsp-pyright setup
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(general-define-key
 :states  '(normal)
 :keymaps '(python-mode-map)
 :prefix  "SPC m"
 "b" 'python-shell-send-buffer
 "r" 'run-python
 )
