(setq sql-package-list '(sqlformat))
(dolist (package sql-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))


(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-u1" "-U1" "-W5"))
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
(provide 'init-sql)
