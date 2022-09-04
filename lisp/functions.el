(defun kill-buffer-and-frame-or-window ()
  "kills current buffer and its window. When its window is the
only one in the current frame, kill the frame instead"
  (interactive)
  (when (kill-buffer)
    (if (one-window-p)
        (delete-frame)
      (delete-window))))

(defun clear-repl ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (cond
     ((equal major-mode 'eshell-mode)
      (eshell-send-input))
     (t (comint-send-input)))))

(defun clear-repl-hook ()
  (evil-local-set-key 'normal (kbd "C-c C-l") 'clear-repl)
  (evil-local-set-key 'insert (kbd "C-c C-l") 'clear-repl))

(defun line-ref-to-kill-ring ()
  "Copy to the kill ring a string in the format \"file-name:line-number\"
for the current buffer's file name, and the line number at point."
  (interactive)
  (let* ((absolute-line-ref (format "%s:%d"
                                    (buffer-file-name)
                                    (save-restriction
                                      (widen) (line-number-at-pos))))
         (project-root (projectile-project-root))
         (line-ref (if project-root
                       (string-remove-prefix project-root absolute-line-ref)
                     absolute-line-ref)))
    (kill-new line-ref)
    (message line-ref)))

;; creates a new eshell buffer
(defun new-eshell ()
  (interactive)
  (eshell t))
