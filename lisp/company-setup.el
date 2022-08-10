(use-package company
  :hook
    (after-init . global-company-mode)
  :custom
    (company-idle-delay 0)
  :config
    ;; better autocompletion
    (use-package company-prescient
      :config
      (company-prescient-mode))
    ;; displays a help popup window
    (use-package company-quickhelp
      :config
        (company-quickhelp-mode))
    (push '(company-capf company-yasnippet) company-backends))

;; company completion popup.  I am using Meta because Control is
;; already bound
(general-define-key
  :states  '(insert)
  :keymaps '(company-active-map override)
    "M-h" 'helm-company
    "M-j" 'company-select-next
    "M-k" 'company-select-previous
    "M-l" 'company-complete
)
