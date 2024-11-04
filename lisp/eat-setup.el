(use-package eat
  :straight (eat :type   git
                 :host   codeberg
                 :repo   "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el"))))

(defun eat-top ()
  (interactive)
  (switch-to-buffer (eat-make "top" "top")))

(define-key global-map [remap remapme-top] 'eat-top)
