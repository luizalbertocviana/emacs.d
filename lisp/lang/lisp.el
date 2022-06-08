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

(defhydra lisp-mode-eval-hydra (:columns 4 :exit t)
  "lisp eval"
  ("e" sly-eval-last-expression "last expression")
  ("f" sly-eval-defun "defun")
  ("i" sly-interactive-eval "interactively")
  ("r" sly-eval-region "region")
  ("p" sly-pprint-eval-last-expression "pretty print")
  ("v" sly-edit-value "edit value")
  ("u" sly-undefine-function "undefine function")
)

(defhydra lisp-mode-compile-hydra (:columns 4 :exit t)
  "lisp compile"
  ("f" sly-compile-defun "defun")
  ("l" sly-compile-and-load-file "compile and load file")
  ("c" sly-compile-file "file")
  ("r" sly-compile-region "region")
)

(defhydra lisp-mode-expand-hydra (:columns 4 :exit t)
  "lisp expand"
  ("e" sly-expand-1 "dwim")
  ("m" sly-macroexpand-1 "macro")
  ("M" sly-macroexpand-all "macro fully")
  ("c" sly-compiler-macroexpand-1 "compiler macro")
  ("C" sly-compiler-macroexpand "compiler macro fully")
  ("s" sly-format-string-expand "format string")
)

(defhydra lisp-mode-find-hydra (:columns 4 :exit t)
  "lisp find"
  ("d" sly-edit-definition "definition")
  ("b" sly-pop-find-definition-stack "go back")
  ("r" sly-edit-uses "references")
  ("c" sly-who-calls "callers")
  ("C" sly-calls-who "callees")
  ("R" sly-who-references "global variable references")
  ("B" sly-who-binds "global variable bindings")
  ("a" sly-who-sets "global variable assignments")
  ("m" sly-who-macroexpands "macro usages")
  ("s" sly-who-specializes "class specializations")
)

(defhydra lisp-mode-documentation-hydra (:columns 4 :exit t)
  "lisp documentation"
  ("m" sly-info "sly manual")
  ("s" sly-describe-symbol "symbol")
  ("f" sly-describe-function "function")
  ("a" sly-apropos "apropos")
  ("A" sly-apropos-all "apropos all")
  ("p" sly-apropos-package "apropos package")
  ("h" sly-hyperspec-lookup "hyperspec")
  ("F" hyperspec-lookup-format "hyperspec format character")
  ("r" hyperspec-lookup-reader-macro "hyperspec reader macro")
)

(defhydra lisp-mode-connections-hydra (:columns 4 :exit t)
  "lisp connections"
  ("a" sly-abort-connection "abort current attempt")
  ("l" sly-list-connections "list")
  ("n" sly-next-connection "next")
  ("p" sly-prev-connection "previous")
  ("c" sly-connect "connect")
  ("d" sly-disconnect "disconnect from current connection")
  ("D" sly-disconnect-all "disconnect all connections")
  ("t" sly-list-threads "list threads")
)

(defhydra lisp-mode-tracing-hydra (:columns 4 :exit t)
  "lisp tracing"
  ("t" sly-toggle-trace-fdefinition "toggle")
  ("u" sly-untrace-all "untrace all")
  ("T" sly-trace-dialog "trace dialog")
)

(defhydra lisp-mode-process-hydra (:columns 4 :exit t)
  "lisp process"
  ("i" sly-interrupt "interrupt")
  ("r" sly-restart-inferior-lisp "restart")
  ("s" sly-mrepl-sync "sync")
  ("c" sly-cd "change directory")
  ("d" sly-pwd "current directory")
)

(defhydra lisp-mode-repl-hydra (:columns 4 :exit t)
  "lisp repl"
  ("s" sly "start")
  ("S" sly-mrepl "select")
  ("n" sly-mrepl-new "new")
  ("r" sly-mrepl-sync "sync")
)

(defhydra lisp-mode-hydra (:columns 4 :exit t)
  "lisp mode"
  ("e" lisp-mode-eval-hydra/body "eval")
  ("E" lisp-mode-expand-hydra/body "expand")
  ("c" lisp-mode-compile-hydra/body "compile")
  ("C" lisp-mode-connections-hydra/body "connections")
  ("d" lisp-mode-documentation-hydra/body "documentation")
  ("D" sly-disassemble-symbol "disassemble")
  ("f" lisp-mode-find-hydra/body "find")
  ("i" sly-inspect "inspect")
  ("l" sly-load-file "load file")
  ("o" sly-scratch "scratch")
  ("p" lisp-mode-process-hydra/body "lisp process")
  ("r" lisp-mode-repl-hydra/body "repl")
  ("t" lisp-mode-tracing-hydra/body "tracing")
)

(general-define-key
 :states '(normal)
 :keymaps '(lisp-mode-map)
 "SPC m" 'lisp-mode-hydra/body)

(general-define-key
   :states '(normal)
   :keymaps 'sly-trace-dialog-mode-map
   "t" 'sly-trace-dialog-toggle-trace
   "r" 'sly-trace-dialog-fetch-status
   "R" 'sly-trace-dialog-fetch-traces
   "c" 'sly-trace-dialog-clear-fetched-traces)

(general-define-key
 :states '(normal)
 :keymaps '(sly-macroexpansion-minor-mode-map)
 "e" 'sly-macroexpand-1-inplace
 "q" 'sly-temp-buffer-quit
 "u" 'sly-macroexpand-undo)

(general-define-key
 :states '(normal)
 :keymaps '(sly-xref-mode-map)
 "RET" 'sly-show-xref
 "r" 'sly-recompile-xref
 "R" 'sly-recompile-all-xrefs)

(general-define-key
 :states '(normal)
 :keymaps '(sly-connection-list-mode-map)
 "RET" 'sly-goto-connection
 "d" 'sly-connection-list-make-default
 "q" 'sly-temp-buffer-quit
 "r" 'sly-update-connection-list
 "R" 'sly-restart-connection-at-point)

(general-define-key
 :states '(normal)
 :keymaps '(sly-mrepl-mode-map)
 "RET" 'sly-mrepl-return
 "TAB" 'sly-mrepl-indent-and-complete-symbol
 "K" 'sly-mrepl-previous-input-or-button
 "J" 'sly-mrepl-next-input-or-button
 "s" 'isearch-backward)

(general-define-key
 :states '(normal)
 :keymaps '(sly-inspector-mode-map)
 "RET" 'sly-inspector-operate-on-point
 "d" 'sly-inspector-describe-inspectee
 "e" 'sly-inspector-eval
 "v" 'sly-inspector-toggle-verbose
 "p" 'sly-inspector-pop
 "n" 'sly-inspector-next
 "r" 'sly-inspector-reinspect
 "H" 'sly-inspector-history
 "q" 'sly-inspector-quit
 "y" 'sly-mrepl-copy-part-to-repl
 "J" 'forward-button
 "K" 'backward-button)

(general-define-key
 :states '(normal)
 :keymaps '(sly-db-mode-map)
 :prefix "SPC m"
 "C" 'sly-db-continue
 "a" 'sly-db-abort
 "c" 'sly-db-recompile-frame-source
 "d" 'sly-db-disassemble
 "e" 'sly-db-eval-in-frame
 "i" 'sly-db-inspect-in-frame
 "I" 'sly-db-inspect-condition
 "j" 'sly-db-details-down
 "k" 'sly-db-details-up
 "p" 'sly-db-pprint-eval-in-frame
 "q" 'sly-db-quit
 "r" 'sly-db-restart-frame
 "R" 'sly-db-return-from-frame
 "s" 'sly-db-show-frame-source
 "t" 'sly-db-toggle-details
 )
