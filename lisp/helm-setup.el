(use-package helm
  :config
  (helm-mode))

(use-package helm-ls-git)

(use-package helm-lsp)

(define-key global-map [remap execute-extended-command] 'helm-M-x)

(define-key helm-map (kbd "C-a") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-h") 'helm-keyboard-quit)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-l") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-m") 'helm-toggle-visible-mark)
