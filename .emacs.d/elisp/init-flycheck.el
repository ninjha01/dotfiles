(use-package 
  flycheck 
  :ensure t 
  :config 
  (add-to-list 'flycheck-checkers 'python-flake8 t)
  (setq flycheck-flake8rc "~/dotfiles/python/.flake8")
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc python/python-pylint))
  ;; Doesn't seem to work
  ;; (setq flycheck-navigation-minimum-level 'error)
  (flycheck-define-checker proselint
    "Flycheck checker using Proselint. See URL `http://proselint.com/'." 
    :command ("proselint" "--json" "-") 
    :standard-input t 
    :error-parser flycheck-proselint-parse-errors 
    :modes (fundamental-mode text-mode markdown-mode gfm-mode message-mode rst-mode))
  :init (global-flycheck-mode)
  :bind
  (:map flycheck-mode-map
	("C-c e" . flycheck-next-error)
	("C-c C-e" . 'flycheck-list-errors)))

;; (flycheck-define-checker
;;     python-mypy "" 
;;     :command ("mypy" "--ignore-missing-imports" source-original) 
;;     :error-patterns ((error
;; 		      line-start
;; 		      (file-name) ":" line ": error:" (message) line-end)) 
;;     :modes python-mode)

;; (add-to-list 'flycheck-checkers 'python-mypy t)
;; (flycheck-add-next-checker 'python-flake8 'python-mypy)

(global-set-key (kbd "C-c e") 'flycheck-next-error)
(global-set-key (kbd "C-c C-e") 'flycheck-next-error)

(provide 'init-flycheck)
