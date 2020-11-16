(setq company-package-list '(company company-go company-lsp company-shell company-web company-c-headers))
(dolist (package company-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company (define-key company-active-map (kbd "C-n") #'company-select-next) 
		      (define-key company-active-map (kbd "C-p") #'company-select-previous))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Speed up prompt
(setq company-idle-delay 0)


(provide 'init-company)
