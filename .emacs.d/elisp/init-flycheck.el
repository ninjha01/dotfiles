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

(global-set-key (kbd "C-c e") 'flycheck-next-error)
(global-set-key (kbd "C-c C-e") 'flycheck-next-error)

(provide 'init-flycheck)
