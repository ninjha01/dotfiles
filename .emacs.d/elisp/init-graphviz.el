(setq graphviz-package-list '(graphviz-dot-mode))
(dolist (package graphviz-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(define-key graphviz-dot-mode-map (kbd "C-c C-c") 'graphviz-dot-preview)
(provide 'init-graphviz)
