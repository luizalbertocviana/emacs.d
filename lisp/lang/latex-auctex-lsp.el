(use-package tex-mode
  :straight auctex
  :hook
    (LaTeX-mode . TeX-fold-mode)
    (LaTeX-mode . TeX-source-correlate-mode)
    (LaTeX-mode . lsp)
  :config
    (with-eval-after-load 'tex
      (push '(output-pdf "Zathura") TeX-view-program-selection))
    (add-hook 'LaTeX-mode-hook 'TeX-fold-buffer t)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (add-hook 'after-save-hook 'TeX-fold-buffer t t))))

;; bibtex searching
(use-package biblio
  :commands (biblio-lookup))

(general-define-key
 :states  '(normal visual)
 :keymaps 'LaTeX-mode-map
 :prefix  "SPC m"
 "B" 'TeX-command-master
 "b" 'TeX-command-run-all
 "e" 'LaTeX-environment
 "v" 'TeX-view
 )
;; bibtex mode
(general-define-key
 :states  '(normal)
 :keymaps 'bibtex-mode-map
 :prefix  "SPC m"
 "s" 'biblio-lookup
 )
;; biblio selection mode
(general-define-key
 :states  '(normal)
 :keymaps 'biblio-selection-mode-map
 "i" 'biblio--selection-insert
 "y" 'biblio--selection-copy-quit
 "h" 'biblio-kill-buffers
 )
