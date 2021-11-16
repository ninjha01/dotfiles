(setq lsp-package-list '(lsp-java lsp-mode lsp-ui lsp-python-ms use-package))
(dolist (package lsp-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))


(use-package 
    lsp-mode 
  :config (setq read-process-output-max (* 1024 1024) lsp-idle-delay 0.5 ;; "Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range."
		lsp-enable-symbol-highlighting t lsp-enable-snippet nil	;; Not supported by company capf, which is the recommended company backend
		lsp-pyls-plugins-flake8-enabled t gc-cons-threshold 100000000 ;; "Adjust gc-cons-threshold. The default setting is too low for lsp-mode’s needs due to the fact that client/server communication generates a lot of memory/garbage."
		lsp-headerline-breadcrumb-enable nil ;; Don't show headerline
		lsp-completion-show-kind nil ;; Don't show completion kind
		lsp-completion-show-detail nil ;; Don't show Completion item detail
		)
  (lsp-register-custom-settings '(
				  ;; Python
				  ("pyls.plugins.pyls_mypy.enabled" t t) 
				  ("pyls.plugins.pyls_mypy.live_mode" nil t) 
				  ("pyls.plugins.pyls_black.enabled" t t) 
				  ("pyls.plugins.pyls_isort.enabled" t t)
				  ;; Disable these as they're duplicated by flake8
				  ("pyls.plugins.pycodestyle.enabled" nil t) 
				  ("pyls.plugins.mccabe.enabled" nil t) 
				  ("pyls.plugins.pyflakes.enabled" nil t))) 
  :hook ((python-mode . lsp) 
	 (java-mode . lsp) 
	 (lsp-mode . lsp-enable-which-key-integration)))

(setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniforge/base/envs/")
(setq flycheck-flake8rc "~/dotfiles/python/.flake8")
(defalias 'workon 'pyvenv-workon)


;; Lsp UI
(use-package 
    lsp-ui 
  :config (setq lsp-ui-sideline-enable nil ;; don't show sdeline diagnostics
		;; lsp-ui-sideline-show-hover t
		;; lsp-ui-sideline-delay 0.5
		lsp-ui-doc-delay 5 lsp-ui-doc-position 'bottom lsp-ui-doc-alignment 'frame
		lsp-ui-doc-header nil lsp-ui-doc-include-signature t lsp-ui-doc-use-childframe t) 
  :commands lsp-ui-mode)

(provide 'init-lsp)
