;; mode line setup
(setq-default mode-line-format
      (list
       ;; evil state
       '(:eval evil-mode-line-tag)
       ;; directory name
       " "
       '(:eval (let ((project-root (projectile-project-root)))
                 (if project-root
                     (concat (projectile-project-name)
                             " - "
                             (string-remove-prefix project-root default-directory))
                   default-directory)))
       ;; buffer name
       '(:eval (if (buffer-modified-p) " %b* " " %b "))
       ;; spacing to right align the remaining items (no idea how this
       ;; works)
       '(:eval
          (propertize " " 'display
                      `((space :align-to (- (+ right right-fringe right-margin) ,(+ 3 (string-width mode-name)))))))
       ;; the following items are right aligned
       ;; major mode
       " %m "))
