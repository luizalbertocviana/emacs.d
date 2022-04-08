(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(add-hook 'typescript-mode-hook 'lsp)
(add-hook 'typescript-mode-hook (lambda ()
                                  (tide-mode)
                                  (tide-restart-server)))

(general-define-key
 :states '(normal)
 :keymaps '(typescript-mode-map)
 :prefix "SPC m"
 "E"   'tide-project-errors
 "F"   'tide-format
 "R"   'tide-references
 "d"   'tide-documentation-at-point
 "e"   'tide-error-at-point
 "f"   'tide-fix
 "i"   'tide-organize-imports
 "j"   'tide-jsdoc-template
 "r f" 'tide-rename-file
 "r s" 'tide-rename-symbol
 "s l" 'tide-list-servers
 "s r" 'tide-restart-server
 "s v" 'tide-verify-setup)
