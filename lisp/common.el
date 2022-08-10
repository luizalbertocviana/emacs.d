;; manage matching pairs
(use-package smartparens
  :config
    ;; the following package is installed automatically
    (use-package smartparens-config :straight nil)
    (smartparens-global-mode t)
    ;; highlights matching pairs
    (show-smartparens-global-mode 1))

(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char))
  :defer nil ;; dont defer so we can add our functions to hooks 
  :config (smart-hungry-delete-add-default-hooks))

;; spell checking
(use-package flyspell :defer t)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :commands (flycheck-mode))

;; colorscheme setup
(use-package doom-themes
  :config
    (load-theme 'doom-tomorrow-night t))

;; colorful delimiters (useful for editing lisp languages)
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook
    (emacs-lisp-mode . rainbow-delimiters-mode))

;; projectile
(use-package projectile
  :custom
    (projectile-completion-system 'ivy)
  :config
    (projectile-mode +1))

;; version control hints in fringe
(use-package diff-hl
  :config
    (global-diff-hl-mode))

;; displays keybindings
(use-package which-key
  :custom
    (which-key-max-display-columns 4)
  :config
    (which-key-mode))

;; hydra setup
(use-package hydra)

;; yasnippet
(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode 1))

;; for some reason it is not being automatically loaded anymore. This
;; loads it manually as it is required by magit
(use-package project)

;; magit
(use-package magit
  :commands (magit-status magit-dispatch))

;; lsp mode
(use-package lsp-mode
    :custom
      (lsp-restart 'auto-restart)
      (lsp-ui-doc-show-with-cursor t)
      (lsp-ui-sideline-show-code-actions t)
      (lsp-prefer-flymake nil)
    :hook
      (lsp-mode . lsp-enable-which-key-integration)
    :config
      (use-package lsp-ui))

(use-package dap-mode
  :hook
    (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode)
)

;; keybinding management
(use-package general)

;; direnv integration
(when (executable-find "direnv")
  (use-package direnv
    :config
    (direnv-mode)))

;; writable grep buffers
(use-package wgrep)

;; sync PATH from shell with emacs exec-path
(use-package exec-path-from-shell
  :config
    (exec-path-from-shell-initialize))
