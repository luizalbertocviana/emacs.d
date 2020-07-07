;;; evil-args-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "evil-args" "evil-args.el" (0 0 0 0))
;;; Generated autoloads from evil-args.el

(autoload 'evil-backward-arg "evil-args" "\
Move the cursor backward COUNT arguments.

\(fn COUNT)" t nil)

(autoload 'evil-forward-arg "evil-args" "\
Move the cursor forward COUNT arguments.

\(fn COUNT)" t nil)
 (autoload 'evil-inner-arg "evil-args")
 (autoload 'evil-outer-arg "evil-args")

(autoload 'evil-jump-out-args "evil-args" "\
Move the cursor out of the nearest enclosing matching pairs.

\(fn COUNT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-args" '("evil-args-")))

;;;***

(provide 'evil-args-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-args-autoloads.el ends here
