(use-package term
  :ensure t
  :init
  (defun term-toggle () 
    "Toggles term between line mode and char mode" 
    (interactive) 
    (if (term-in-line-mode) 
	(term-char-mode) 
      (term-line-mode)))
  :bind
  (:map term-mode-map
	("C-c C-j" . term-toggle)
	("C-c t" . (lambda () 
		     (interactive) 
		     (shell) 
		     (rename-uniquely))))
  :config
  (setq shell-file-name "zsh"))



(provide 'init-shell)
