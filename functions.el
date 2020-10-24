;; the functions below are intended to be used in i3wm keybindings
;; with emacs --eval

;; creates a new eshell buffer
(defun new-eshell ()
  (eshell t))

;; creates dired buffer in current dir
(defun dired-current-dir ()
  (dired "./"))

(defun dired-dwim-target-directory ()
    (let* ((get-buffer-dir (lambda (buffer)
                             (with-current-buffer buffer
                               (and (eq major-mode 'dired-mode)
                                    (dired-current-directory)))))
           (this-dir (funcall get-buffer-dir (current-buffer)))
           (other-dir (some (lambda (buffer)
                              (let ((buffer-dir (funcall get-buffer-dir buffer)))
                                (and (not (equal buffer-dir this-dir))
                                     buffer-dir)))
                            (buffer-list))))
      (if dired-dwim-target
          (or other-dir this-dir)
        this-dir)))
