(use-package 
  docker 
  :ensure t)

(use-package   
  dockerfile-mode
  :after (docker)
  :ensure t 
  :mode (("Dockerfile\\'" . dockerfile-mode)))

(provide 'init-docker)
