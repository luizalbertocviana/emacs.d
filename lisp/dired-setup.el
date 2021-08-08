(use-package dired-aux :straight nil)

;; dired file manager
(use-package dired-x :straight nil
  :hook
    (dired-mode . dired-omit-mode)
  :custom
    ;; do what i mean, makes operations intuitive when there are two
    ;; open dired buffers
    (dired-dwim-target t)  
    ;; dired does not change last modified timestamp when copying
    ;; files
    (dired-copy-preserve-time t)
    ;; human readable sizes
    (dired-listing-switches "-alh")
  :config
    ;; this command is disabled by default, but I like this better
    ;; than 'dired-find-file
    (put 'dired-find-alternate-file 'disabled nil)
    ;; this prevents dot files from being listed (this cannot be put
    ;; into a :custom section (dont know why))
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
    ;; this makes dired list directories first
    (use-package ls-lisp :straight nil
      :custom
        (ls-lisp-dirs-first t)
        (ls-lisp-use-insert-directory-program nil)))

;; colorful dired
(use-package diredfl
  :config
    (diredfl-global-mode))

;; facilities to use subtrees in dired
(use-package dired-subtree)

(general-define-key
  :states  '(normal)
  :keymaps 'dired-mode-map
    "H" 'dired-hide-details-mode
    "J" 'dired-next-marked-file
    "K" 'dired-prev-marked-file
    "M" 'dired-unmark
    "N" 'evil-ex-search-previous
    "O" 'dired-subtree-toggle
    "U" 'dired-unmark-all-marks
    "h" (lambda () (interactive) (find-alternate-file ".."))
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "l" 'dired-find-alternate-file
    "m" 'dired-mark
    "n" 'evil-ex-search-next
    "o" 'browse-url-of-dired-file
    "s" 'evil-ex-search-forward
    "u" 'dired-undo
    "y" 'dired-copy-filename-as-kill
)

(defhydra dired-search-hydra (dired-mode-map nil :columns 4 :exit t)
  "dired search"
  ("g" find-grep-dired               "grep")
  ("r" dired-do-query-replace-regexp "query replace regexp")
  ("s" dired-do-isearch-regexp       "search regexp")
)

(defhydra dired-hydra (dired-mode-map nil :columns 4 :exit t)
  "dired"
  ("D" dired-diff                                   "diff")
  ("S" dired-do-symlink                             "symlink")
  ("T" dired-toggle-marks                           "toggle marks")
  ("U" dired-upcase                                 "upcase")
  ("Z" dired-do-compress                            "compress")
  ("c" wdired-change-to-wdired-mode                 "edit directory")
  ("d" dired-do-delete                              "delete")
  ("g" dired-do-chgrp                               "change group")
  ("h" dired-omit-mode                              "toggle hide")
  ("i" (lambda () (interactive) (image-dired "./")) "image dired")
  ("l" dired-downcase                               "downcase")
  ("m" dired-do-chmod                               "change mode")
  ("n" dired-create-directory                       "new directory")
  ("o" dired-do-chown                               "change owner")
  ("r" dired-do-rename                              "rename")
  ("s" dired-search-hydra/body                      "search")
  ("t" dired-do-async-shell-command                 "run command")
  ("y" dired-do-copy                                "copy")
  ("z" dired-do-compress-to                         "compress to")
)

(general-define-key :states '(normal)
                    :keymaps 'dired-mode-map
                    "SPC m" 'dired-hydra/body)

(general-define-key
  :states  '(normal)
  :keymaps '(image-dired-thumbnail-mode-map)
    "h" 'image-dired-backward-image
    "l" 'image-dired-forward-image
    "j" 'image-dired-next-line
    "k" 'image-dired-previous-line
)
