(use-package 
  lsp-mode 
  :config
  (setq read-process-output-max (* 1024 1024)
	lsp-idle-delay 0.5 ;; "Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range."
	lsp-enable-symbol-highlighting t
	lsp-enable-snippet nil	;; Not supported by company capf, which is the recommended company backend
	 gc-cons-threshold 100000000 ;; "Adjust gc-cons-threshold. The default setting is too low for lsp-mode’s needs due to the fact that client/server communication generates a lot of memory/garbage."
	lsp-headerline-breadcrumb-enable nil ;; Don't show headerline
	lsp-completion-show-kind nil ;; Don't show completion kind
	lsp-completion-show-detail nil
	lsp-disabled-clients '(pylsp pyls)
	lsp-diagnostics-flycheck-enable t
	) ;; Don't show Completion item detail
  
  :hook
  ((java-mode . lsp) 
   (lsp-mode . lsp-enable-which-key-integration)))


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
