(setq web-package-list '(web-mode tide js-comint prettier-js))
(dolist (package web-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(require 'web-mode)
(setq web-mode-enable-auto-closing t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-engine "django")
;;;; Use web-mode for...

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook 
	  (lambda () 
	    (when (string-equal "tsx" (file-name-extension buffer-file-name)) 
	      (setup-tide-mode))))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))


;;;; Tide Mode
(use-package tide
  :config
  (defun setup-tide-mode () 
    (interactive) 
    (tide-setup) 
    (flycheck-mode +1) 
    (setq flycheck-check-syntax-automatically '(save mode-enabled)) 
    (eldoc-mode +1) 
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :hook
  ((web-mode . setup-tide-mode)))

(use-package prettier-js
  :hook
  ((web-mode . prettier-js-mode)
   (js2-mode . prettier-js-mode)
   (tide-mode . prettier-js-mode)))

(provide 'init-web)
