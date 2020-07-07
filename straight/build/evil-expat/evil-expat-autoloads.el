;;; evil-expat-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "evil-expat" "evil-expat.el" (0 0 0 0))
;;; Generated autoloads from evil-expat.el

(eval-after-load 'evil '(progn (evil-ex-define-cmd "rev[erse]" 'evil-expat-reverse) (autoload 'evil-expat-reverse "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "remove" 'evil-expat-remove) (autoload 'evil-expat-remove "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "rename" 'evil-expat-rename) (autoload 'evil-expat-rename "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "gblame" 'evil-expat-gblame) (autoload 'evil-expat-gblame "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "gremove" 'evil-expat-gremove) (autoload 'evil-expat-gremove "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "tyank" 'evil-expat-tyank) (autoload 'evil-expat-tyank "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "tput" 'evil-expat-tput) (autoload 'evil-expat-tput "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "diff-orig" 'evil-expat-diff-orig) (autoload 'evil-expat-diff-orig "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "colo[rscheme]" 'evil-expat-colorscheme) (autoload 'evil-expat-colorscheme "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "gdiff" 'evil-expat-gdiff) (autoload 'evil-expat-gdiff "evil-expat" nil t)))

(eval-after-load 'evil '(progn (evil-ex-define-cmd "gread" 'evil-expat-gread) (autoload 'evil-expat-gread "evil-expat" nil t)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-expat" '("evil-expat-")))

;;;***

(provide 'evil-expat-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-expat-autoloads.el ends here
