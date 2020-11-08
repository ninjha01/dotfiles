(setq projectile-package-list '(projectile))
(dolist (package projectile-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; Required for .projectile ignore files
;; (setq projectile-indexing-method 'native)
;; temp change back to alien
(setq projectile-indexing-method 'alien)

;; For react native projects
(add-to-list 'projectile-globally-ignored-directories "Pods")
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories ".mypy_cache")
(projectile-mode 1)

;; for ansible submodules
(setq projectile-git-submodule-command nil)


(provide 'init-projectile)
