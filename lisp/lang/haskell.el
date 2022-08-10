(use-package haskell-mode
  :hook
    (haskell-mode . interactive-haskell-mode))

(use-package lsp-haskell)

(add-hook 'haskell-mode-hook 'lsp t nil)

(defun haskell-clear-repl ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (haskell-interactive-mode-return)))

(defun haskell-clear-repl-hook ()
  (evil-local-set-key 'normal (kbd "C-c C-l") 'haskell-clear-repl)
  (evil-local-set-key 'insert (kbd "C-c C-l") 'haskell-clear-repl))

(add-hook 'haskell-interactive-mode-hook 'haskell-clear-repl-hook)

(general-define-key
 :states  '(normal)
 :keymaps '(haskell-mode-map)
 :prefix  "SPC m"
 "C" 'haskell-cabal-visit-file
 "I" 'haskell-mode-format-imports
 "b" 'haskell-process-cabal-build
 "c" 'haskell-process-cabal
 "i" 'haskell-process-do-info
 "l" 'haskell-process-load-file
 "r" 'haskell-interactive-switch
 "t" 'haskell-process-do-type
 )
