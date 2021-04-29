(setq magit-package-list '(magit forge magit-topgit))
(dolist (package magit-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)) ;; auto-refresh

(with-eval-after-load 'magit 
  (require 'forge))
(require 'git-link)
(global-set-key (kbd "C-c g l") 'git-link)
(provide 'init-magit)
