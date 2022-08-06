(use-package helm
  :config
  (helm-mode))

(use-package helm-ls-git)

(use-package helm-lsp)

(use-package helm-projectile)

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
  :keymaps 'grep-mode-map
  :prefix "SPC m"
    "c" 'wgrep-change-to-wgrep-mode
)

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
