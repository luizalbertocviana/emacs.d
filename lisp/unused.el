(use-package company-fish
      :straight (company-fish :type   git
                              :flavor melpa
                              :host   github
                              :repo   "CeleritasCelery/company-fish")
      :config
        (when (executable-find "fish")
          (add-to-list 'company-backends 'company-fish)
          (add-hook 'shell-mode-hook 'company-mode)
          (add-hook 'eshell-mode-hook 'company-mode)))
