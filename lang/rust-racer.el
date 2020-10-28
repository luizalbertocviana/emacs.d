(use-package rust-mode
  :config
    (use-package rustic
      :custom
        (rustic-lsp-client nil))
    (use-package racer
      :hook
        (rustic-mode . racer-mode)
        (racer-mode . eldoc-mode)))

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
 "d" 'racer-find-definition
 "e" 'next-error
 "f" 'rustic-format-buffer
 "h" 'racer-describe
 "m" 'rustic-popup
 "r" 'rustic-recompile
 "t" 'rustic-cargo-test
 )
