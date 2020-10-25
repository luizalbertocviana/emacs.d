(general-define-key
 :states  '(normal)
 :keymaps '(clojure-mode-map)
 :prefix "SPC m"
 "L" 'cider-load-all-files
 "b" 'cider-load-buffer
 "d" 'cider-debug-defun-at-point
 "e" 'cider-macroexpand-1
 "f" 'cider-eval-defun-at-point
 "h" 'cider-doc
 "i" 'cider-inspect
 "l" 'cider-load-file
 "r" 'cider-jack-in
 )
