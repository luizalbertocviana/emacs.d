(add-hook 'c++-mode-hook 'lsp t nil)

;; gdb setup
(setq gdb-display-io-nopopup t)

(general-define-key
 :states  '(normal)
 :keymaps '(c++-mode-map)
 :prefix  "SPC m"
 "b" 'compile
 "d" 'gdb
 "e" 'next-error
 "r" 'recompile
 )
;; gdb mode
(general-define-key
 :states  '(normal)
 :keymaps '(gud-mode-map)
 :prefix "SPC m"
 "b" 'gdb-frame-breakpoints-buffer
 "d" 'gdb-frame-disassembly-buffer
 "i" 'gdb-frame-io-buffer
 "l" 'gdb-frame-locals-buffer
 "m" 'gdb-frame-memory-buffer
 "s" 'gdb-frame-stack-buffer
 )
