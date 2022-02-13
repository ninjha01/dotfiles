(setq dockerfile-package-list '(docker dockerfile-mode))
(dolist (package dockerfile-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(provide 'init-docker)
