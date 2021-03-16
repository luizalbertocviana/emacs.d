(setq python-indent-offset 4)

;; jupyter setup
(use-package jupyter)

;; lsp-pyright setup
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(general-define-key
 :states  '(normal)
 :keymaps '(python-mode-map)
 :prefix  "SPC m"
 "b" 'jupyter-eval-buffer
 "d" 'jupyter-inspect-at-point
 "f" 'jupyter-eval-defun
 "r" 'jupyter-run-repl
 )
