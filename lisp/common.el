;; manage matching pairs
(use-package smartparens
  :config
    ;; the following package is installed automatically
    (use-package smartparens-config :straight nil)
    (smartparens-global-mode t)
    ;; highlights matching pairs
    (show-smartparens-global-mode 1))

;; spell checking
(use-package flyspell :defer t)

;; colorscheme setup
(use-package doom-themes
  :config
    (load-theme 'doom-tomorrow-night t))

;; start screen
(use-package dashboard
  :custom
    (dashboard-startup-banner 'logo)
    (dashboard-center-content t)
    (dashboard-set-file-icons nil)
    (dashboard-set-footer     nil)
    (dashboard-items          '((projects . 10)))
    (initial-buffer-choice    (lambda () (get-buffer "*dashboard*")))
  :config
    (dashboard-setup-startup-hook))

;; colorful delimiters (useful for editing lisp languages)
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook
    (emacs-lisp-mode . rainbow-delimiters-mode))

;; ivy, providing some completion facilities to certain emacs contexts
(use-package ivy
  :custom
    (ivy-use-virtual-buffers t)
    (enable-recursive-minibuffers t)
  :config
    (ivy-mode 1)
    (use-package counsel
      :config
        (counsel-mode 1))
    (use-package ivy-posframe
      :custom
        (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
      :config
        (ivy-posframe-mode 1)))

;; projectile
(use-package projectile
  :custom
    (projectile-completion-system 'ivy)
  :config
    (projectile-mode +1))

(use-package dired-aux :straight nil)

;; dired file manager
(use-package dired-x :straight nil
  :hook
    (dired-mode . dired-omit-mode)
  :custom
    ;; do what i mean, makes operations intuitive when there are two
    ;; open dired buffers
    (dired-dwim-target t)  
    ;; dired does not change last modified timestamp when copying
    ;; files
    (dired-copy-preserve-time t)
    ;; human readable sizes
    (dired-listing-switches "-alh")
  :config
    ;; this command is disabled by default, but I like this better
    ;; than 'dired-find-file
    (put 'dired-find-alternate-file 'disabled nil)
    ;; this prevents dot files from being listed (this cannot be put
    ;; into a :custom section (dont know why))
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
    ;; this makes dired list directories first
    (use-package ls-lisp :straight nil
      :custom
        (ls-lisp-dirs-first t)
        (ls-lisp-use-insert-directory-program nil)))

;; displays keybindings
(use-package which-key
  :config
    (which-key-mode))

;; autocompletion
(use-package company
  :hook
    (after-init . global-company-mode)
  :custom
    (company-idle-delay 0)
    (company-transformers '(company-sort-prefer-same-case-prefix))
  :config
    ;; displays a help popup window
    (use-package company-quickhelp
      :config
        (company-quickhelp-mode))
    ;; fish-like completion for eshell
    (use-package company-fish
      :straight (company-fish :type   git
                              :flavor melpa
                              :host   github
                              :repo   "CeleritasCelery/company-fish")
      :config
        (when (executable-find "fish")
          (add-to-list 'company-backends 'company-fish)
          (add-hook 'shell-mode-hook 'company-mode)
          (add-hook 'eshell-mode-hook 'company-mode)))
    ;; yasnippet
    (use-package yasnippet
      :config
        (use-package yasnippet-snippets)
        (yas-global-mode 1)
        (push '(company-capf :with company-yasnippet) company-backends)))

;; better company/ivy autocompletion
(use-package prescient
  :config
    ;; ivy integration
    (use-package ivy-prescient
      :config
        (ivy-prescient-mode))
    ;; company integration
    (use-package company-prescient
      :config
      (company-prescient-mode))
    ;; remember autocompletion choices made in previous emacs sessions
    (prescient-persist-mode))

;; magit
(use-package magit
  :commands (magit-status magit-dispatch))

;; lsp mode
(use-package lsp-mode
    :custom
      (lsp-restart 'auto-restart)
    :hook
      (lsp-mode . lsp-enable-which-key-integration)
    :config
      (use-package lsp-ui))

;; simple and effective interface to google translation service
(use-package google-translate)
