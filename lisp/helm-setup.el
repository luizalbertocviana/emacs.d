(use-package helm)

(general-define-key
 :states  '(normal)
 :keymaps '(override)
 :prefix  "SPC"
 "h"   '(:ignore t :which-key "helm")
 "h F" 'helm-find
 "h H" 'helm-run-external-command
 "h I" 'helm-imenu-in-all-buffers
 "h a" 'helm-apropos
 "h f" 'helm-multi-files
 "h g" 'helm-google-suggest
 "h h" 'helm-M-x
 "h i" 'helm-imenu
 "h m" 'helm-man-woman
 "h p" 'helm-list-emacs-process
 "h r" 'helm-regexp
 "h s" 'helm-occur
 "h t" 'helm-top
)

(define-key helm-map (kbd "C-a") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-h") 'helm-keyboard-quit)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "C-l") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-m") 'helm-toggle-visible-mark)
