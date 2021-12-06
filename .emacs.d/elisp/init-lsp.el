
(setq lsp-package-list '(lsp-java lsp-mode lsp-ui use-package))
(dolist (package lsp-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))


(use-package 
  lsp-mode 
  :config
  (setq read-process-output-max (* 1024 1024)
	lsp-idle-delay 0.5 ;; "Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range."
	lsp-enable-symbol-highlighting t lsp-enable-snippet nil	;; Not supported by company capf, which is the recommended company backend
	lsp-pyls-plugins-flake8-enabled t gc-cons-threshold 100000000 ;; "Adjust gc-cons-threshold. The default setting is too low for lsp-mode’s needs due to the fact that client/server communication generates a lot of memory/garbage."
	lsp-headerline-breadcrumb-enable nil ;; Don't show headerline
	lsp-completion-show-kind nil ;; Don't show completion kind
	lsp-completion-show-detail nil
	lsp-disabled-clients '(pylsp pyls)
	) ;; Don't show Completion item detail
  :hook ((java-mode . lsp) 
	 (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map python-mode-map
	      ("M-S-<right>" . python-indent-shift-right)
	      ("M-S-<left>" . python-indent-shift-left)))



;; python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package
  blacken
  :demand t
  :hook ((python-mode . blacken-mode)))
(use-package 
  pyvenv 
  :demand t 
  :config (setq pyvenv-workon "emacs")  ; Default venv
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniforge/base/envs/") 
  (defalias 'workon 'pyvenv-workon) 
  (pyvenv-tracking-mode 1)) ; Automatically use pyvenv-workon via dir-locals

(use-package 
  flycheck 
  :ensure t 
  :config (flycheck-define-checker python-mypy "" 
				   :command ("mypy" "--ignore-missing-imports" source-original) 
				   :error-patterns ((error 
						     line-start
						     (file-name)
						     ":"
						     line
						     ": error:"
						     (message)
						     line-end)) 
				   :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-flake8 t) 
  (add-to-list 'flycheck-checkers 'python-mypy t)
  (flycheck-add-next-checker 'python-mypy 'python-flake8) 
  (setq flycheck-flake8rc "~/dotfiles/python/.flake8") 
  :init (global-flycheck-mode))

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
