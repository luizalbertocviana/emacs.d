(use-package gnus :straight nil
  :defer t
  :custom
    (gnus-auto-center-summary nil)
    (gnus-blocked-images      "ads")
    (gnus-ignored-newsgroups  "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
    (gnus-novice-user         t)
    (gnus-show-all-headers    nil)
  :hook
    (gnus-select-group . gnus-group-set-timestamp)
  :config
    (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
    ;; setting gmail
    (setq smtpmail-smtp-server  "smtp.gmail.com")
    (setq smtpmail-smtp-service 587)
    (setq gnus-select-method
          '(nnimap "gmail"
                   (nnimap-address "imap.gmail.com")
                   (nnimap-server-port "imaps")
                   (nnimap-stream ssl))))
