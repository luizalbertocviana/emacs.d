(use-package helm)

(use-package helm-ls-git)

(general-define-key
 :states  '(normal)
 :keymaps '(override)
 :prefix  "SPC"
 "h"   '(:ignore t :which-key "helm")
 "h a" 'helm-apropos
 "h h" 'helm-M-x
 "h i" 'helm-imenu-in-all-buffers
 "h m" 'helm-man-woman
 "h r" 'helm-regexp
 "h t" 'helm-top
)

(define-key helm-map (kbd "C-a") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-h") 'helm-keyboard-quit)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-l") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-m") 'helm-toggle-visible-mark)
