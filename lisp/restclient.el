(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; this is a contribution from https://github.com/bigodel
(defun web-restclient-get-header-from-response (header)
  "Get HEADER from the response buffer of restclient.
HEADER should be just the name of the header, e.g.
  \"content-type\" (it is case insensitive)."
  (let* ((case-fold-search t)
         (search-string (format "// %s: " header))
         (match (string-match search-string
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
    (goto-char match)
    (forward-char (length search-string))
    (buffer-substring-no-properties (point)
                                    (progn
                                      (move-end-of-line 1)
                                      (point)))))

(general-define-key
 :states  '(normal)
 :keymaps '(restclient-mode-map)
 :prefix  "SPC m"
   "c" 'restclient-copy-curl-command
   "j" 'restclient-jump-next
   "k" 'restclient-jump-prev
   "r" 'restclient-http-send-current-stay-in-window
 )
