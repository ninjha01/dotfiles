(use-package magit
  :ensure t
  :bind
  (:map global-map
	("C-x g" . magit-status)
	("C-c g" . magit-file-dispatch)
	("C-c d" . magit-dispatch)
	("C-c b" . magit-blame))
  :config
  (setq magit-save-repository-buffers 'dontask)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package forge
  :ensure t
  :after (magit))

(use-package forge
  :ensure t
  :after (git-link))

(provide 'init-git)
