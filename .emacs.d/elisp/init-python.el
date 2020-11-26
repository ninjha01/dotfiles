(setq py-package-list '(blacken elpy jedi py-isort))
(dolist (package py-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

;;;; elpy
(setq python-shell-interpreter "python3" elpy-rpc-python-command "python3"
      python-shell-interpreter-args "-i")
(setenv "WORKON_HOME" "/usr/local/Caskroom/miniconda/base/envs/")
(add-hook 'elpy-mode-hook 
	  '(lambda () 
	     (when (eq major-mode 'python-mode) 
	       (add-hook 'before-save-hook 'elpy-black-fix-code))))

(provide 'init-python)
