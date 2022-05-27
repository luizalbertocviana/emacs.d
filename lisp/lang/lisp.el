(use-package sly
  :hook
    (lisp-mode . sly-mode)
  :custom
    (inferior-lisp-program "ros run"))

(add-hook 'lisp-mode-hook 'lisp-correct-closing-quote t t)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode t nil)

(defun eval-last-expression-inplace ()
  "evaluates expression and puts its result just after it"
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively #'sly-eval-last-expression)))

(general-define-key
 :states  '(normal)
 :keymaps '(lisp-mode-map sly-mrepl-mode-map)
 :prefix  "SPC m"
 "A" 'sly-disassemble-symbol
 "C" 'sly-calls-who
 "H" 'sly-hyperspec-lookup
 "R" 'sly-restart-inferior-lisp
 "S" 'sly-stickers-replay
 "T" 'sly-trace-dialog
 "a" 'sly-apropos-all
 "b" 'sly-compile-and-load-file
 "c" 'sly-edit-uses
 "d" 'sly-edit-definition
 "e" 'eval-last-expression-inplace
 "f" 'sly-compile-defun
 "h" 'sly-describe-symbol
 "i" 'sly-inspect
 "l" 'sly-load-file
 "m" 'sly-expand-1
 "n" 'sly-mrepl-new
 "o" 'sly-scratch
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
 :states  '(normal)
 :keymaps '(sly-db-mode-map)
 "a" 'sly-db-abort
 "c" 'sly-db-continue
 "p" 'sly-db-pprint-eval-in-frame
 "q" 'sly-db-quit
 )
(general-define-key
 :states '(normal)
 :keymaps '(sly-db-mode-map)
 :prefix "SPC m"
 "R" 'sly-db-return-from-frame
 "c" 'sly-db-recompile-frame-source
 "d" 'sly-db-disassemble
 "r" 'sly-db-restart-frame
 "s" 'sly-db-show-frame-source
 )
