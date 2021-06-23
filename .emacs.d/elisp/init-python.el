(setq py-package-list '(blacken elpy jedi py-isort))
(dolist (package py-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(add-hook 'python-mode-hook 
	  (lambda () 
	    (elpy-enable)))


;;;; elpy
(setq elpy-rpc-virtualenv-path 'current)
(setq python-shell-interpreter "python3" elpy-rpc-python-command "python3"
      python-shell-interpreter-args "-i")
(setenv "WORKON_HOME" "/Users/njha/miniconda3/envs")
(setq flycheck-flake8rc "~/dotfiles/python/.flake8")
(require 'elpy)
(add-hook 'elpy-mode-hook 
	  '(lambda () 
	     (add-hook 'before-save-hook 'elpy-black-fix-code 'make-it-local)))
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(defalias 'workon 'pyvenv-workon)

(defun jedi-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (jedi:goto-definition)
        (error (elpy-rgrep-symbol
                (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
(define-key elpy-mode-map (kbd "M-.") 'jedi-goto-definition-or-rgrep)


(provide 'init-python)
