(general-define-key
 :states  '(normal visual)
 :keymaps '(sql-mode-map)
 :prefix  "SPC m"
 "b" 'sql-send-buffer
 "p" 'sql-send-paragraph
 "r" 'sql-show-sqli-buffer
 "v" 'sql-send-region
 )

(general-define-key
 :states  '(normal visual)
 :keymaps '(sql-interactive-mode-map)
 :prefix  "SPC m"
 "l" 'sql-list-all
 "t" 'sql-list-table
 )
