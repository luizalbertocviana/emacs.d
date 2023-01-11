(use-package lsp-java)

(add-hook 'java-mode-hook 'lsp)

(defvar enable-lombok nil
  "whether to enable lombok")

(defvar lombok-path nil
  "when non-nil, takes precedence over lombok autodetected path")

(setq enable-lombok t)

(defun lombok-path-from-naven-local-repository ()
  (let ((lombok-m2-dir (file-truename "~/.m2/repository/org/projectlombok/lombok/")))
    (when (file-exists-p lombok-m2-dir)
      (let* ((version-regex (rx (seq (one-or-more digit)
                                     "."
                                     (one-or-more digit)
                                     "."
                                     (one-or-more digit))))
             (lombok-m2-versions (directory-files lombok-m2-dir nil version-regex))
             (lombok-m2-version (car lombok-m2-versions)))
        (concat lombok-m2-dir
                lombok-m2-version
                "/lombok-"
                lombok-m2-version
                ".jar")))))

(when enable-lombok
  (let ((path (or lombok-path
                  (lombok-path-from-naven-local-repository))))
    (setq lsp-java-vmargs
          (cons (concat "-javaagent:" path)
                lsp-java-vmargs))
    (setq lsp-java-vmargs
          (cons (concat "-Xbootclasspath/a:" path)
                lsp-java-vmargs))))
