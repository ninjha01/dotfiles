(setq avy-package-list '(avy))
(dolist (package avy-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))


;; Open and name shell
(global-set-key (kbd "M-s") 'avy-goto-char-2)
(provide 'init-avy)
