(use-package company-auctex
  :commands (company-auctex-init))
(use-package company-math
  :after (company-auctex)
  :config
  (push 'company-math-symbols-latex company-backends)
  (push 'company-latex-commands     company-backends))
(use-package company-reftex
  :after (company-auctex)
  :custom
  (company-reftex-max-annotation-length 80))

(use-package tex-site
  :straight auctex
  :mode ("\\.tex\\'" . latex-mode)
  :hook
    ;; synctex and stuff
    (LaTeX-mode . TeX-source-correlate-mode)
    ;; prepares to pretty-print some symbols
    (LaTeX-mode . TeX-fold-mode)
  :custom
    (TeX-auto-save t)
    (TeX-parse-self t)
    (TeX-master nil)
    (TeX-source-correlate-method 'synctex)
    (TeX-source-correlate-start-server t)
  :config
    (defcustom LaTeX-inhibited-auto-fill-environments
      '("tabular" "tikzpicture")
      "latex environments where auto-fill is disabled")
    (defun LaTeX-inhibited-auto-fill-environment-p (environment)
      (member environment LaTeX-inhibited-auto-fill-environments))
    ;; only auto-fills when outside of inhibited environments
    (defun LaTeX-limited-auto-fill ()
      (let ((environment (LaTeX-current-environment)))
        (unless (LaTeX-inhibited-auto-fill-environment-p environment)
          (do-auto-fill))))
    (add-hook 'LaTeX-mode-hook (lambda () (setq-local auto-fill-function 'LaTeX-limited-auto-fill)))
    ;; pretty-prints some symbols and commands when entering and
    ;; saving latex files
    (add-hook 'LaTeX-mode-hook 'TeX-fold-buffer t)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (add-hook 'after-save-hook 'TeX-fold-buffer t t)))
    ;; general setup: reftex, company, pdf mode and reftex isearch
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (reftex-mode)
                (company-auctex-init)
                (setq-local company-backends
                            (append '((company-reftex-labels company-reftex-citations))
                                    company-backends))
                (TeX-PDF-mode t)
                (reftex-isearch-minor-mode)))
    ;; use zathura as a pdf viewer
    (with-eval-after-load 'tex
      (add-to-list 'TeX-view-program-selection
                   '(output-pdf "Zathura")))
    ;; adds some facilities like folding
    (use-package latex-extra
      :hook
        (LaTeX-mode . latex-extra-mode)
      :config
        (define-key evil-normal-state-local-map (kbd "TAB") 'latex/hide-show)))

;; LaTeX references and citations
(use-package reftex
  :commands (reftex-mode)
  :custom
    (reftex-plug-into-AUCTeX t))

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
 "l" 'reftex-label
 "m" 'TeX-insert-macro
 "p" 'preview-buffer
 "s" 'LaTeX-section
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
