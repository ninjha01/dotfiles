(setq magit-package-list '(magit forge magit-topgit git-link))
(dolist (package magit-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(global-set-key (kbd "C-c d") 'magit-dispatch)
(global-set-key (kbd "C-c b") 'magit-blame)
(with-eval-after-load 'magit-mode (add-hook 'after-save-hook 'magit-after-save-refresh-status t)) ;; auto-refresh


(with-eval-after-load 'magit 
  (require 'forge))

(setq magit-save-repository-buffers 'dontask)

(require 'git-link)
(global-set-key (kbd "C-c C-g l") 'git-link)
(eval-after-load 'git-link
  '(progn
     (add-to-list 'git-link-remote-alist
      '("git\\.ginkgobioworks\\.com" git-link-gitlab)) 
     (add-to-list 'git-link-commit-remote-alist
		  '("git\\.ginkgobioworks\\.com" git-link-commit-gitlab))))

(provide 'init-git)
