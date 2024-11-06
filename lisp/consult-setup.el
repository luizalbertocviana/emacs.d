;; Consult users will also want the embark-consult package.
(use-package embark-consult)

(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(use-package consult)

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(defun consult-ripgrep-thing-at-point ()
  (interactive)
  (let ((thing-to-search-for (thing-at-point 'symbol)))
    (consult-ripgrep nil thing-to-search-for)))

(defun consult-ripgrep-dwim (arg)
  (interactive "P")
  (xref-push-marker-stack)
  (if arg
      (consult-ripgrep)
    (consult-ripgrep-thing-at-point)))

(define-key global-map [remap switch-to-buffer] 'consult-buffer)
(define-key global-map [remap list-bookmarks] 'consult-bookmark)
(define-key global-map [remap recentf-open-files] 'consult-recent-file)
(define-key global-map [remap remapme-imenu-in-all-buffers] 'consult-imenu-multi)
(define-key global-map [remap imenu] 'consult-imenu)
(define-key global-map [remap projectile-grep] 'consult-ripgrep-dwim)
(define-key global-map [remap grep] 'consult-ripgrep-dwim)
(define-key global-map [remap apropos] 'consult-apropos)
(define-key global-map [remap occur] 'consult-line)
(define-key global-map [remap projectile-switch-project] 'consult-projectile)
(define-key global-map [remap projectile-recentf] 'consult-projectile-recentf)
(define-key global-map [remap projectile-find-file-dwim] 'consult-projectile-find-file)
(define-key global-map [remap next-error] 'consult-compile-error)

(setq completion-in-region-function #'consult-completion-in-region)
