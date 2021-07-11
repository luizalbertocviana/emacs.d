(use-package cider)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode t nil)

(defun start-cider-repl-with-profile ()
  (interactive)
  (let ((profile (read-string "Enter profile name: ")))
    (set-variable 'cider-clojure-cli-aliases profile)
    (cider-jack-in nil)))

(general-define-key
 :states  '(normal)
 :keymaps '(clojure-mode-map)
 :prefix "SPC m"
 "L" 'cider-load-all-files
 "R" 'start-cider-repl-with-profile
 "b" 'cider-load-buffer
 "d" 'cider-debug-defun-at-point
 "e" 'cider-macroexpand-1
 "f" 'cider-eval-defun-at-point
 "h" 'cider-doc
 "i" 'cider-inspect
 "l" 'cider-load-file
 "r" 'cider-jack-in
 )

(general-define-key
 :states  '(normal)
 :keymaps '(cider-repl-mode-map)
 :prefix "SPC m"
 "n" 'cider-repl-set-ns
 )
