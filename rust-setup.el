(use-package rustic
  :custom
    (rustic-lsp-client nil))

(general-define-key
 :states  '(normal)
 :keymaps '(rustic-mode-map)
 :prefix  "SPC m"
 "C" 'rustic-cargo-clippy
 "D a" 'rustic-cargo-add
 "D r" 'rustic-cargo-rm
 "D u" 'rustic-cargo-upgrade
 "b" 'rustic-cargo-run
 "c" 'rustic-cargo-check
 "d" 'xref-find-definitions
 "e" 'next-error
 "f" 'rustic-format-buffer
 "m" 'rustic-popup
 "r" 'rustic-recompile
 "t" 'rustic-cargo-test
 )
