;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :hook
    (scala-mode . flycheck-mode)
    (scala-mode . lsp)
    (lsp-mode . lsp-lens-mode)
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Add metals backend for lsp-mode
(use-package lsp-metals)

(general-define-key
 :states '(normal)
 :keymaps '(scala-mode-map)
 :prefix "SPC m"
   "c" 'sbt-command
   "h" 'sbt-hydra
   "s" 'sbt-start
   "r" 'run-scala
   "R" 'sbt-run-previous-command
)
