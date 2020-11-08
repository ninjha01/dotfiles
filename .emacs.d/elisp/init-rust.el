(setq rust-package-list '(rust-mode cargo flycheck-rust))
(dolist (package rust-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(with-eval-after-load 'rust-mode (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;; Indentation
(add-hook 'rust-mode-hook 
	  (lambda () 
	    (setq indent-tabs-mode nil)))

;;;; Rustfmt
(setq rust-format-on-save t)

;;;; Cargo minor mode
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(provide 'init-rust)
