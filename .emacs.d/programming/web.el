;; Web Development Configuration

;; Web mode for HTML, JS, TS, JSX, TSX
(use-package web-mode 
  :ensure t 
  :mode (("\\.html\\'" . web-mode) 
         ("\\.tsx\\'" . web-mode) 
         ("\\.jsx\\'" . web-mode) 
         ("\\.js\\'" . web-mode) 
         ("\\.ts\\'" . web-mode) 
         ("\\.cjs\\'" . web-mode) 
         ("\\.mjs\\'" . web-mode) 
         ("\\.json\\'" . web-mode)) 
  :config 
  (setq web-mode-markup-indent-offset 2 
        web-mode-code-indent-offset 2 
        web-mode-css-indent-offset 2 
        web-mode-enable-css-colorization t 
        web-mode-enable-auto-pairing t 
        web-mode-enable-auto-indentation nil 
        web-mode-enable-comment-keywords t 
        web-mode-enable-auto-quoting nil 
        web-mode-enable-current-element-highlight t) 
  :hook (web-mode . (lambda () 
                      (lsp-deferred) 
                      (when (string-equal "tsx" (file-name-extension buffer-file-name)) 
                        (setup-tide-mode)))))

;; TypeScript mode
(use-package typescript-mode 
  :ensure t 
  :config 
  (setq typescript-indent-level 2) 
  (add-hook 'typescript-mode #'subword-mode) 
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; TypeScript Interactive Development Environment
(use-package tide 
  :ensure t 
  :hook (typescript-mode . tide-setup) 
  :bind (:map tide-mode-map
              ;; bind C-<return> to tide fix
              ("C-<return>" . tide-fix)) 
  :init 
  (defun setup-tide-mode () 
    (interactive) 
    (tide-setup) 
    (flycheck-mode +1) 
    (setq flycheck-check-syntax-automatically '(save mode-enabled)) 
    (flycheck-select-checker 'typescript-tide) 
    (eldoc-mode +1) 
    (tide-hl-identifier-mode +1) 
    (company-mode +1)))

(provide 'web)