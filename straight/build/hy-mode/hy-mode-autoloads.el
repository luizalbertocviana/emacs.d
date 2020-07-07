;;; hy-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "hy-base" "hy-base.el" (0 0 0 0))
;;; Generated autoloads from hy-base.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hy-base" '("hy--")))

;;;***

;;;### (autoloads nil "hy-font-lock" "hy-font-lock.el" (0 0 0 0))
;;; Generated autoloads from hy-font-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hy-font-lock" '("inferior-hy-font-lock-kwds" "hy-font-lock-")))

;;;***

;;;### (autoloads nil "hy-jedhy" "hy-jedhy.el" (0 0 0 0))
;;; Generated autoloads from hy-jedhy.el

(autoload 'run-jedhy "hy-jedhy" "\
Startup internal Hy interpreter process, enabling jedhy for `company-mode'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hy-jedhy" '("run-jedhy--pyvenv-post-deactive-hook" "company-hy" "hy-")))

;;;***

;;;### (autoloads nil "hy-mode" "hy-mode.el" (0 0 0 0))
;;; Generated autoloads from hy-mode.el

(add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))

(add-to-list 'interpreter-mode-alist '("hy" . hy-mode))

(autoload 'hy-mode "hy-mode" "\
Major mode for editing Hy files.

\(fn)" t nil)

(autoload 'hy-insert-pdb "hy-mode" "\
Import and set pdb trace at point.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hy-mode" '("hy-" "inferior-hy-mode-syntax-table")))

;;;***

;;;### (autoloads nil "hy-shell" "hy-shell.el" (0 0 0 0))
;;; Generated autoloads from hy-shell.el

(autoload 'inferior-hy-mode "hy-shell" "\
Major mode for Hy inferior process.

\(fn)" t nil)

(autoload 'run-hy "hy-shell" "\
Startup and/or switch to a Hy interpreter process.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hy-shell" '("hy-")))

;;;***

(provide 'hy-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hy-mode-autoloads.el ends here
