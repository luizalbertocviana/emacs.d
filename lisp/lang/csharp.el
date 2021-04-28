(use-package csharp-mode)

(add-hook 'csharp-mode-hook 'lsp t nil)

(add-hook 'csharp-mode-hook
          (lambda ()
            (setq-local compile-command "dotnet build")))
