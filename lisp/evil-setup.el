(use-package evil
  :init
    (setq evil-search-module 'evil-search)
    ;; uses M-x for emacs commands
    (setq evil-ex-complete-emacs-commands nil)
    ;; like vim's 'splitbelow'
    (setq evil-split-window-below t) 
    (setq evil-shift-round nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-move-beyond-eol t)
    ;; evil everywhere: every mode starts at normal state, and no mode
    ;; keymap overrides evil
    (setq evil-want-integration nil)
    (setq evil-emacs-state-modes nil)
    (setq evil-insert-state-modes nil)
    (setq evil-motion-state-modes nil)
    (setq evil-overriding-maps nil)
    (setq evil-intercept-maps nil)
    ;; state indicators appearence
    (setq evil-normal-state-tag   (propertize " NORMAL "   'face '((:background "olive drab" :foreground "black"))))
    (setq evil-emacs-state-tag    (propertize " EMACS "    'face '((:background "purple"     :foreground "black"))))
    (setq evil-insert-state-tag   (propertize " INSERT "   'face '((:background "orange"     :foreground "black"))))
    (setq evil-motion-state-tag   (propertize " MOTION "   'face '((:background "gray"       :foreground "black"))))
    (setq evil-visual-state-tag   (propertize " VISUAL "   'face '((:background "yellow"     :foreground "black"))))
    (setq evil-operator-state-tag (propertize " OPERATOR " 'face '((:background "gray"       :foreground "black"))))
    ;; cursor appearence
    (setq evil-normal-state-cursor '(box "olive drab"))
    (setq evil-emacs-state-cursor '(box "purple"))
    (setq evil-insert-state-cursor '((bar . 2) "orange"))
    (setq evil-motion-state-cursor '(box "gray"))
    (setq evil-visual-state-cursor '(box "yellow"))
  :config
    (evil-mode)
    ;; ex commands, which a vim user is likely to be familiar with
    (use-package evil-expat :defer t)
    ;; visual editing hints
    (use-package evil-goggles
      :config
        (evil-goggles-mode))
    ;; like vim-surround
    (use-package evil-surround
      :config
        (global-evil-surround-mode 1))
    (use-package evil-args
      :config
        (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
        (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))
    ;; this makes M-v selects larger and larger regions in a
    ;; semantic-wise way
    (use-package expand-region
      :config
        (define-key evil-normal-state-map (kbd "M-v") 'er/expand-region)
        (define-key evil-visual-state-map (kbd "M-v") 'er/expand-region))
    ;; some keybindings
    (evil-define-key 'normal 'global
      ;; movements
      "H" "^"
      "J" "}"
      "K" "{"
      "L" "$"
      ;; yanking until end of line
      "Y" "y$"
      ;; macro execution
      "Q" "@q"
      ;; search
      "s" 'evil-ex-search-forward)
    (evil-define-key 'visual 'global
      ;; movements
      "H" "^"
      "J" "}"
      "K" "{"
      "L" "$"
      ;; linewise macro execution
      "Q" (kbd ":norm @q RET"))
    ;; makes TAB indent current line or selected text
    (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
    (define-key evil-visual-state-map (kbd "TAB") 'indent-for-tab-command))

;; vim undo redo
(use-package undo-fu
  :config
    (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
    (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(general-define-key
 :states  '(normal)
 :keymaps '(override)
 :prefix  "g"
 "b" 'xref-go-back
 "f" 'xref-go-forward)
