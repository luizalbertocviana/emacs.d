(use-package rustic
  :custom
    (rustic-lsp-client nil))

(add-hook 'rustic-mode-hook 'lsp t nil)

(defhydra rustic-dependency-hydra (rustic-mode-map nil :columns 4 :exit t)
  ("a" rustic-cargo-add     "add")
  ("r" rustic-cargo-rm      "remove")
  ("u" rustic-cargo-upgrade "upgrade")
)

(defhydra rustic-hydra (rustic-mode-map nil :columns 4 :exit t)
  ("C" rustic-cargo-clippy          "clippy")
  ("D" rustic-dependency-hydra/body "dependencies") ; FIXME
  ("b" rustic-cargo-run             "run")
  ("c" rustic-cargo-check           "check")
  ("d" xref-find-definitions        "find definition")
  ("e" next-error                   "next error")
  ("f" rustic-format-buffer         "format")
  ("r" rustic-recompile             "recompile")
  ("t" rustic-cargo-test            "test")
)

(general-define-key :states '(normal)
                    :keymaps 'rustic-mode-map
                    "SPC m" 'rustic-hydra/body)
