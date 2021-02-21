(use-package sly
  :hook
    (lisp-mode . sly-mode)
  :custom
    (inferior-lisp-program "ros run"))

(add-hook 'lisp-mode-hook 'lisp-correct-closing-quote t t)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode t nil)

(general-define-key
 :states  '(normal)
 :keymaps '(lisp-mode-map sly-mrepl-mode-map)
 :prefix  "SPC m"
 "A" 'sly-disassemble-symbol
 "C" 'sly-calls-who
 "D" 'sly-edit-uses
 "H" 'sly-hyperspec-lookup
 "S" 'sly-stickers-replay
 "T" 'sly-trace-dialog
 "a" 'sly-apropos-all
 "b" 'sly-compile-and-load-file
 "c" 'sly-who-calls
 "d" 'sly-edit-definition
 "e" 'sly-expand-1
 "f" 'sly-compile-defun
 "h" 'sly-describe-symbol
 "i" 'sly-inspect
 "l" 'sly-load-file
 "m" 'sly-who-macroexpands
 "r" 'sly
 "s" 'sly-stickers-dwim
 "t" 'sly-trace-dialog-toggle-trace
 )
(general-define-key
 :states  '(normal)
 :keymaps '(sly-mrepl-mode-map)
 "M-j" 'sly-mrepl-next-prompt
 "M-k" 'sly-mrepl-previous-prompt
 )
(general-define-key
 :states  '(insert)
 :keymaps '(sly-mrepl-mode-map)
 "<down>" 'sly-mrepl-next-input-or-button
 "<up>"   'sly-mrepl-previous-input-or-button
 "RET"    'sly-mrepl-return
 )
(general-define-key
 :states  '(normal)
 :keymaps '(sly-stickers--replay-mode-map)
 "n" 'sly-stickers-replay-next
 "p" 'sly-stickers-replay-prev
 "q" 'kill-buffer-and-window
 )
(general-define-key
 :states '(normal)
 :keymaps '(sly-db-mode-map)
 "J" 'sly-db-details-down
 "K" 'sly-db-details-up
 "a" 'sly-db-abort
 "c" 'sly-db-continue
 "d" 'sly-db-pprint-eval-in-frame
 "e" 'sly-db-eval-in-frame
 "i" 'sly-db-inspect-in-frame
 "q" 'sly-db-quit
 "t" 'sly-db-toggle-details
 "v" 'sly-db-show-frame-source
 )
(general-define-key
 :states '(normal)
 :keymaps '(sly-db-mode-map)
 :prefix "SPC m"
 "R" 'sly-db-return-from-frame
 "c" 'sly-db-recompile-frame-source
 "d" 'sly-db-disassemble
 "r" 'sly-db-restart-frame
 )
