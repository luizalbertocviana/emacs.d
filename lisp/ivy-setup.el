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
        (ivy-posframe-mode 1))
    (use-package ivy-rich
      :config
        (ivy-rich-mode)
        (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)))

;; ivy minibuffer
(general-define-key
  :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
    "C-a" 'ivy-dispatching-done
    "C-b" 'ivy-occur
    "C-h" 'keyboard-escape-quit
    "C-i" 'ivy-insert-current
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line
    "C-l" 'ivy-alt-done
    "M-j" 'ivy-next-history-element
    "M-k" 'ivy-previous-history-element
    "RET" 'ivy-immediate-done)
