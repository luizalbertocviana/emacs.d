(use-package cider)

(use-package helm-cider
  :config
    (helm-cider-mode 1))

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode t nil)

(defun start-cider-repl-with-profile ()
  (interactive)
  (letrec ((profile (read-string "Enter profile name: "))
           (lein-params (concat "with-profile +" profile " repl :headless")))
    (message "lein-params set to: %s" lein-params)
    (set-variable 'cider-lein-parameters lein-params)
    (cider-jack-in nil)))

(defhydra clojure-mode-repl-hydra (:columns 4 :exit t)
  "clojure repl"
  ("c" cider-jack-in "start clojure repl")
  ("C" cider-connect "connect to clojure repl")
  ("j" cider-jack-in-cljs "start clojurescript repl")
  ("J" cider-connect-cljs "connect to clojurescript repl")
  ("b" cider-jack-in-clj&cljs "start both repls")
  ("B" cider-connect-clj&cljs "connect to both repls")
  ("p" start-cider-repl-with-profile "clojure repl with profile")
)

(defhydra clojure-mode-eval-hydra (:collumns 4 :exit t)
  "clojure eval"
  ("d" cider-debug-defun-at-point "debug")
  ("e" cider-eval-defun-at-point "defun")
  ("i" cider-interrupt "interrupt")
  ("p" cider-pprint-eval-defun-at-point "pretty print")
  ("s" cider-eval-last-sexp "sexp")
)

(defhydra clojure-mode-doc-hydra (:collumns 4 :exit t)
  "clojure doc"
  ("c" cider-clojuredocs "clojuredocs")
  ("d" cider-doc "documentation")
  ("a" cider-apropos "apropos")
)

(defhydra clojure-mode-test-hydra (:collumns 4 :exit t)
  "clojure test"
  ("f" cider-test-rerun-failed-tests "run failed tests")
  ("l" cider-test-rerun-test "run last test")
  ("n" cider-test-run-ns-tests "run namespace tests")
  ("p" cider-test-run-project-tests "run project tests")
  ("s" cider-test-show-report "show report")
  ("t" cider-test-run-test "run test")
)

(defhydra clojure-mode-find-hydra (:columns 4 :exit t)
  "clojure find"
  ("d" cider-xref-fn-deps "dependencies")
  ("f" cider-find-var "definition")
  ("n" cider-find-ns "namespace")
  ("r" cider-xref-fn-refs "references")
)

(defhydra clojure-mode-browse-hydra (:columns 4 :exit t)
  "clojure browse"
  ("n" cider-browse-ns-all "namespaces")
  ("s" cider-browse-spec-all "specs")
)

(defhydra clojure-mode-hydra (:columns 4 :exit t)
  "clojure"
  ("B" clojure-mode-browse-hydra/body "browse")
  ("T" cider-toggle-trace-var "toggle tracing")
  ("b" cider-load-buffer "load buffer")
  ("d" clojure-mode-doc-hydra/body "documentation")
  ("e" clojure-mode-eval-hydra/body "eval")
  ("f" clojure-mode-find-hydra/body "find")
  ("i" cider-inspect "inspect")
  ("l" cider-load-file "load file")
  ("m" cider-macroexpand-1 "macroexpand")
  ("n" cider-repl-set-ns "change repl namespace")
  ("o" cider-scratch "open scratch")
  ("r" clojure-mode-repl-hydra/body "repl")
  ("s" cider-apropos "search")
  ("t" clojure-mode-test-hydra/body "test")
  ("u" cider-undef "undef")
  ("v" cider-enlighten-mode "view locals")
)

(general-define-key
 :states  '(normal)
 :keymaps '(clojure-mode-map)
 "SPC m" 'clojure-mode-hydra/body
 )

(general-define-key
 :states  '(normal)
 :keymaps '(cider-repl-mode-map)
 :prefix "SPC m"
 "n" 'cider-repl-set-ns
 )

(general-define-key
 :states  '(normal)
 :keymaps '(cider-stacktrace-mode-map)
 "J"   'cider-stacktrace-next-cause
 "K"   'cider-stacktrace-previous-cause
 "RET" 'cider-stacktrace-jump
 "TAB" 'cider-stacktrace-cycle-current-cause
 )

(general-define-key
 :states  '(normal)
 :keymaps '(cider-browse-ns-mode-map)
 "RET" 'cider-browse-ns-operate-at-point
 "H"   'cider-browse-ns-all
 "d"   'cider-browse-ns-doc-at-point
 "s"   'cider-browse-ns-find-at-point
 )

(general-define-key
 :states  '(normal)
 :keymaps '(cider-browse-spec-mode-map)
 "H"   'cider-browse-spec-all
 "e"   'cider-browse-spec--print-curr-spec-example
 )

(general-define-key
 :states  '(normal)
 :keymaps '(cider-test-report-mode-map)
 "RET" 'cider-test-jump
 "d"   'cider-test-ediff
 "f"   'cider-test-rerun-failed-tests
 "n"   'cider-test-run-ns-tests
 "p"   'cider-test-run-project-tests
 "r"   'cider-test-run-test
 "s"   'cider-test-stacktrace
 )

(general-define-key
 :states  '(normal)
 :keymaps '(cider-inspector-mode-map)
 :prefix "SPC m"
 "d" 'cider-inspector-def-current-val
 "i" 'cider-inspector-operate-on-point
 "j" 'cider-inspector-next-page
 "k" 'cider-inspector-prev-page
 "p" 'cider-inspector-pop
 "r" 'cider-inspector-refresh
)

; cider debugger settings (adapted from evil-collections)

(add-hook 'cider-mode-hook 'evil-normalize-keymaps)
(add-hook 'cider--debug-mode-hook 'evil-normalize-keymaps)

(general-define-key
 :states  '(normal)
 :keymaps '(cider--debug-mode-map)
 "n" (lambda () (interactive) (cider-debug-mode-send-reply ":next"))
 "c" (lambda () (interactive) (cider-debug-mode-send-reply ":continue"))
 "o" (lambda () (interactive) (cider-debug-mode-send-reply ":out"))
 "q" (lambda () (interactive) (cider-debug-mode-send-reply ":quit"))
 "e" (lambda () (interactive) (cider-debug-mode-send-reply ":eval"))
 "j" (lambda () (interactive) (cider-debug-mode-send-reply ":inject"))
 "i" (lambda () (interactive) (cider-debug-mode-send-reply ":inspect"))
 "l" (lambda () (interactive) (cider-debug-mode-send-reply ":locals"))
 )
