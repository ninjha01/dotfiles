(setq py-package-list '(blacken elpy jedi py-isort))
(dolist (package py-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(add-hook 'python-mode-hook 
	  (lambda () 
	    (elpy-mode 1)))


;;;; elpy
(setq python-shell-interpreter "python3" elpy-rpc-python-command "python3"
      python-shell-interpreter-args "-i")
(setenv "WORKON_HOME" "/usr/local/Caskroom/miniconda/base/envs/")
(setq flycheck-flake8rc "~/dotfiles/python/.flake8")
(require 'elpy)
(add-hook 'elpy-mode-hook 
	  '(lambda () 
	     (add-hook 'before-save-hook 'elpy-black-fix-code 'make-it-local)))
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(defalias 'workon 'pyvenv-workon)


(provide 'init-python)
