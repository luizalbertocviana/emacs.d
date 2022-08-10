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
