(setq flycheck-package-list '())
(dolist (package flycheck-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'go-mode 'flycheck-mode)
(add-hook 'python-mode 'flycheck-mode)

(flycheck-define-checker proselint
  "Flycheck checker using Proselint. See URL `http://proselint.com/'." 
  :command ("proselint" "--json" "-") 
  :standard-input t 
  :error-parser flycheck-proselint-parse-errors 
  :modes (fundamental-mode text-mode markdown-mode gfm-mode message-mode rst-mode))


(setq-default flycheck-disabled-checkers '(python/python-pylint))

;; python
(flycheck-define-checker python-mypy "" 
			 :command ("mypy" "--ignore-missing-imports" 
				   source-original) 
			 :error-patterns ((error 
					   line-start
					   (file-name)
					   ":"
					   line
					   ": error:"
					   (message)
					   line-end)) 
			 :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)
(flycheck-add-next-checker 'python-flake8 'python-mypy)

(setq flycheck-config-files ".flake8")

(global-set-key (kbd "C-c e") 'flycheck-next-error)
(global-set-key (kbd "C-c C-e") 'flycheck-next-error)

(provide 'init-flycheck)
