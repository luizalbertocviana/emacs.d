(use-package haskell-mode
  :hook
    (haskell-mode . interactive-haskell-mode))

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
