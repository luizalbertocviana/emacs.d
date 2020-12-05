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

(general-define-key
 :states  '(normal)
 :keymaps '(override)
 :prefix  "SPC"
 "g" 'gnus)

(general-define-key
 :states  '(normal)
 :keymaps '(gnus-group-mode-map)
 "J" 'gnus-group-next-unread-group
 "K" 'gnus-group-prev-unread-group
 "M" 'gnus-group-unmark-group
 "U" 'gnus-group-unmark-all-groups
 "l" 'gnus-topic-select-group
 "m" 'gnus-group-mark-group
 "q" 'gnus-group-exit
 "r" 'gnus-group-get-new-news
 )
(general-define-key
 :states  '(normal)
 :keymaps '(gnus-group-mode-map)
 :prefix  "SPC m"
 "L"   'gnus-group-list-all-groups
 "R"   'gnus-group-catchup-current-all
 "S"   'gnus-group-enter-server-mode
 "d"   'gnus-topic-kill-group
 "k"   'gnus-group-list-killed
 "l"   'gnus-group-list-groups
 "s"   'gnus-group-unsubscribe-current-group
 "y"   'gnus-topic-yank-group
 "z"   'gnus-group-list-zombies
 "m"   'gnus-group-mail
 "t d" 'gnus-topic-delete
 "t h" 'gnus-topic-unindent
 "t l" 'gnus-topic-indent
 "t m" 'gnus-topic-move-group
 "t n" 'gnus-topic-create-topic
 "t r" 'gnus-topic-rename
 "t s" 'gnus-topic-sort-groups-by-alphabet
 )
(general-define-key
 :states  '(normal)
 :keymaps '(gnus-summary-mode-map)
 "J" 'gnus-summary-next-unread-subject
 "K" 'gnus-summary-prev-unread-subject
 "Q" 'gnus-summary-exit-no-update
 "h" (lambda () (interactive) (gnus-summary-prev-page 1 nil))
 "l" (lambda () (interactive) (gnus-summary-next-page 1 nil t))
 "q" 'gnus-summary-exit
 "r" 'gnus-summary-prepare
 )
(general-define-key
 :states  '(normal)
 :keymaps '(gnus-summary-mode-map)
 :prefix  "SPC m"
 "C"   'gnus-summary-cancel-article
 "D"   'gnus-summary-enter-digest-group
 "R"   'gnus-summary-very-wide-reply-with-original
 "a"   'gnus-article-save-part
 "b"   'gnus-summary-set-bookmark
 "d"   'gnus-summary-delete-article
 "f"   'gnus-summary-mail-forward
 "m R" 'gnus-summary-catchup
 "m U" 'gnus-summary-clear-mark-forward
 "m d" 'gnus-summary-mark-as-dormant
 "m r" 'gnus-summary-mark-as-read-forward
 "m u" 'gnus-summary-tick-article
 "p"   'gnus-summary-print-article
 "r"   'gnus-summary-reply-with-original
 "s m" 'gnus-summary-mail-other-window
 "s n" 'gnus-summary-news-other-window
 "v A" 'gnus-summary-limit-to-address
 "v a" 'gnus-summary-limit-to-author
 "v b" 'gnus-summary-limit-to-bodies
 "v r" 'gnus-summary-limit-to-unread
 "v s" 'gnus-summary-limit-to-subject
 "v t" 'gnus-summary-limit-to-age
 "w"   'gnus-summary-save-article-file
 "W"   'gnus-summary-write-article-file
 )
(general-define-key
 :states  '(normal)
 :keymaps '(gnus-server-mode-map)
 "R" 'gnus-server-regenerate-server
 "c" 'gnus-server-edit-server
 "d" 'gnus-server-kill-server
 "o" 'gnus-server-read-server
 "p" 'gnus-server-yank-server
 "q" 'gnus-server-exit
 "r" 'gnus-server-scan-server
 "v" 'gnus-server-show-server
 "y" 'gnus-server-copy-server
 )
(general-define-key
 :states  '(normal)
 :keymaps '(gnus-server-mode-map)
 :prefix  "SPC m"
 "C" 'gnus-server-close-all-servers
 "O" 'gnus-server-open-all-servers
 "c" 'gnus-server-close-server
 "d" 'gnus-server-deny-server
 "l" 'gnus-server-list-servers
 "n" 'gnus-server-add-server
 "o" 'gnus-server-open-server
 "u" 'gnus-server-remove-denials
 )
