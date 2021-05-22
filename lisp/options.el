;; we do not use package.el
(setq package-enable-at-startup nil)
;; this keeps emacs from appending variables to the end of this file
(setq package--init-file-ensured t)

;; resizing makes no sense if frame is always used maximized
(setq frame-inhibit-implied-resize t)

;; makes the simplest mode possible our initial one
(setq initial-major-mode 'fundamental-mode)

;; inhibit useless and old-school startup screen
(setq inhibit-startup-screen t )
(menu-bar-mode -1)
(tool-bar-mode -1)

;; disable scroll bars in every frame
(toggle-scroll-bar -1)

;; dont use dialog boxes, use minibuffer instead
(setq use-dialog-box nil)

;; confirm before exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; keeps mouse away when typing
(mouse-avoidance-mode 'banish)

;; makes cursor stop blinking
(blink-cursor-mode -1)

;; highlights current line
(global-hl-line-mode 1)

;; indents using spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; breaks lines when they become too long ...
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))
;; ... but this is only applied to comments when in prog mode
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local comment-auto-fill-only-comments t)
            (auto-fill-mode 1)
          ))

;; activates abbrev mode in text mode (useful for correcting typos)
(add-hook 'text-mode-hook 'abbrev-mode)

;; keeps current line from within a margin, regarding both top and
;; bottom lines
(setq scroll-margin 20)

;; font
(add-to-list 'default-frame-alist '(font . "Fira Code-10"))

;; line numbers setup
;; relative line numbering
(setq display-line-numbers-type 'relative)
;; minimum width (in characters) of line number column
(setq-default display-line-numbers-width 4)
;; activate line number mode globally
(global-display-line-numbers-mode)

;; auto save
(add-hook 'text-mode-hook 'auto-save-mode)
(add-hook 'prog-mode-hook 'auto-save-mode)

;; open files in last edited position
(save-place-mode 1)

;; in prog mode, enable hideshow mode (see keybindings)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; makes docview scrolling change document pages
(setq doc-view-continuous t)

;; gdb setup
(setq gdb-display-io-nopopup t)
