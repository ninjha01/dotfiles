(use-package projectile
  :ensure t
  :after (magit)
  :config
  (add-to-list 'projectile-globally-ignored-directories "Pods")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".mypy_cache")
  ;; Required for .projectile ignore files
  ;; (setq projectile-indexing-method 'native)
  (setq projectile-indexing-method 'alien)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; for ansible submodules
  (setq projectile-git-submodule-command nil)
  (setq projectile-switch-project-action 'magit-status))
(projectile-mode 1)

(provide 'init-projectile)
