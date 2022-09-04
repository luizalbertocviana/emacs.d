(use-package company-fish
      :straight (company-fish :type   git
                              :flavor melpa
                              :host   github
                              :repo   "CeleritasCelery/company-fish")
      :config
        (when (executable-find "fish")
          (add-to-list 'company-backends 'company-fish)
          (add-hook 'shell-mode-hook 'company-mode)
          (add-hook 'eshell-mode-hook 'company-mode)))

;; start screen
(use-package dashboard
  :custom
    (dashboard-startup-banner 'logo)
    (dashboard-center-content t)
    (dashboard-set-file-icons nil)
    (dashboard-set-footer     nil)
    (dashboard-items          '((projects . 10)))
    (initial-buffer-choice    (lambda () (get-buffer "*dashboard*")))
  :config
    (dashboard-setup-startup-hook))

;; dashboard
(general-define-key
  :states  '(normal)
  :keymaps 'dashboard-mode-map
    "p" (general-key "p" :state 'emacs)
    "l" 'dashboard-return
)

;; creates dired buffer in current dir
(defun dired-current-dir ()
  (dired "./"))

(defun dired-dwim-target-directory ()
  (let* ((get-buffer-dir (lambda (buffer)
                           (with-current-buffer buffer
                             (and (eq major-mode 'dired-mode)
                                  (dired-current-directory)))))
         (this-dir (funcall get-buffer-dir (current-buffer)))
         (other-dir (seq-some (lambda (buffer)
                                (let ((buffer-dir (funcall get-buffer-dir buffer)))
                                  (and (not (equal buffer-dir this-dir))
                                       buffer-dir)))
                              (buffer-list))))
    (if dired-dwim-target
        (or other-dir this-dir)
      this-dir)))
