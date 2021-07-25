(setq magit-package-list '(magit forge magit-topgit git-link))
(dolist (package magit-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(with-eval-after-load 'magit-mode (add-hook 'after-save-hook 'magit-after-save-refresh-status t)) ;; auto-refresh

(defun git-add-current-buffer () 
  "Adds (with force) the file from the current buffer to the git repo" 
  (interactive) 
  (shell-command (concat "git add -f " (shell-quote-argument buffer-file-name)))
  )

(with-eval-after-load 'magit 
  (require 'forge))

(require 'git-link)
(global-set-key (kbd "C-c C-g l") 'git-link)
(eval-after-load 'git-link
  '(progn
     (add-to-list 'git-link-remote-alist
      '("git\\.ginkgobioworks\\.com" git-link-gitlab)) 
     (add-to-list 'git-link-commit-remote-alist
		  '("git\\.ginkgobioworks\\.com" git-link-commit-gitlab))))

(provide 'init-git)
