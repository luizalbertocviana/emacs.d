(defun intercalate (lst elem)
  (cond
   ((not (listp lst)) nil)
   ((length< lst 2) lst)
   (t
    (let ((head (car lst))
          (tail (cdr lst)))
      (append (list head elem)
              (intercalate tail elem))))))

(defun eshell-treat-command-name (command-name)
  (if (string-prefix-p "#<function" command-name)
      (replace-regexp-in-string (rx (seq "#<function "
                                         (group (zero-or-more (not blank)))
                                         ">"))
                                (rx (backref 1))
                                command-name)
    command-name))

(defun eshell-last-input ()
  (let ((command (eshell-treat-command-name eshell-last-command-name))
        (args (apply #'concat (intercalate eshell-last-arguments " "))))
    (concat command " " args)))

(defvar eshell-local-dir-history-filename ".eshell-local-history")

(defun eshell-write-last-input-to-local-dir-history ()
  (let ((last-input (eshell-last-input)))
    (unless (string= last-input "")
      (append-to-file (concat last-input "\n")
                      nil
                      eshell-local-dir-history-filename))))

(defun eshell-local-dir-history-capf ()
  (let ((inputs (split-string (with-temp-buffer
                                (insert-file-contents-literally eshell-local-dir-history-filename)
                                (buffer-string))
                              "\n"))
        (bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            inputs
            :exclusive 'no))))

(add-hook 'eshell-post-command-hook
          'eshell-write-last-input-to-local-dir-history)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (cons #'eshell-local-dir-history-capf
                              completion-at-point-functions))))
