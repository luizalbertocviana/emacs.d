(use-package helm
  :config
  (helm-mode))

(use-package helm-ls-git)

(use-package helm-lsp)

(use-package helm-projectile
  :straight (helm-projectile :type   git
                             :flavor melpa
                             :host   github
                             :repo   "luizalbertocviana/helm-projectile"))

(use-package helm-company
  :after company
  :custom
    (helm-company-candidate-number-limit nil))

(setq helm-grep-ag-command (concat "rg"
                                   " --color=never"
                                   " --smart-case"
                                   " --no-heading"
                                   " --line-number %s %s %s"))

(setq helm-grep-file-path-style 'relative)

(add-hook 'helm-grep-mode-hook 'grep-mode)

(general-define-key
  :states  '(normal)
  :keymaps 'wgrep-mode-map
  :prefix "SPC m"
    "a" 'wgrep-abort-changes
    "d" 'wgrep-mark-deletion
    "f" 'wgrep-finish-edit
    "q" 'wgrep-exit
    "s" 'wgrep-save-all-buffers
    "u" 'wgrep-remove-change
    "U" 'wgrep-remove-all-change
)

(define-key global-map [remap shell-command] 'helm-run-external-command)
(define-key global-map [remap remapme-google-suggest] 'helm-google-suggest)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap remapme-imenu-in-all-buffers] 'helm-imenu-in-all-buffers)
(define-key global-map [remap imenu] 'helm-imenu)
(define-key global-map [remap list-processes] 'helm-list-emacs-process)
(define-key global-map [remap remapme-top] 'helm-top)
(define-key global-map [remap woman] 'helm-man-woman)
(define-key global-map [remap apropos] 'helm-apropos)
(define-key global-map [remap switch-to-buffer] 'helm-buffers-list)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap remapme-find] 'helm-find)
(define-key global-map [remap list-bookmarks] 'helm-bookmarks)
(define-key global-map [remap find-file] 'helm-multi-files)
(define-key global-map [remap projectile-grep] 'helm-projectile-grep)
(define-key global-map [remap projectile-find-file-dwim] 'helm-projectile-find-file-dwim)
(define-key global-map [remap remapme-browse-project] 'helm-browse-project)
(define-key global-map [remap projectile-recentf] 'helm-projectile-recentf)
(define-key global-map [remap projectile-switch-project] 'helm-projectile-switch-project)
(define-key global-map [remap grep] 'helm-do-grep-ag)
(define-key global-map [remap remapme-regexp] 'helm-regexp)
(define-key global-map [remap remapme-lsp-global-workspace-symbol] 'helm-lsp-global-workspace-symbol)
(define-key global-map [remap lsp-execute-code-action] 'helm-lsp-code-actions)
(define-key global-map [remap remapme-lsp-workspace-symbol] 'helm-lsp-workspace-symbol)

(define-key global-map [remap execute-extended-command] 'helm-M-x)

(define-key helm-map (kbd "C-a") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-h") 'helm-keyboard-quit)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-l") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-m") 'helm-toggle-visible-mark)

;; makes helm buffer always appear at the bottom bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))
