(setq crux-package-list '(crux))
(dolist (package crux-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(global-set-key (kbd "C-x C-r") 'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c o") 'crux-open-with)

(global-set-key (kbd "C-c ,") 'crux-find-user-custom-file)
(provide 'init-crux)
