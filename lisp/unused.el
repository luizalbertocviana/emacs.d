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

;; start screen
(use-package dashboard
  :custom
    (dashboard-startup-banner 'logo)
    (dashboard-center-content t)
    (dashboard-set-file-icons nil)
    (dashboard-set-footer     nil)
    (dashboard-items          '((projects . 10)))
    (initial-buffer-choice    (lambda () (get-buffer "*dashboard*")))
  :config
    (dashboard-setup-startup-hook))
