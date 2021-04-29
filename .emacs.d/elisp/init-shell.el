(setq shell-package-list '())
(dolist (package shell-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(require 'term)
(defun term-toggle () 
  "Toggles term between line mode and char mode" 
  (interactive) 
  (if (term-in-line-mode) 
      (term-char-mode) 
    (term-line-mode)))
(define-key term-mode-map (kbd "C-c C-j") 'term-toggle)

;; Open and name shell
(global-set-key (kbd "C-c t") 
		(lambda () 
		  (interactive) 
		  (shell) 
		  (rename-uniquely)))


(provide 'init-shell)
