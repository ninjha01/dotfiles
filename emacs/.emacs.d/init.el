;;; init.el --- Emacs configuration entry point
;;; Commentary:
;;; Code:
;; Modular Emacs configuration
;; Main init file - loads modular components

;; Add all subdirectories to load path
(add-to-list 'load-path "~/.emacs.d/core")
(add-to-list 'load-path "~/.emacs.d/ui")
(add-to-list 'load-path "~/.emacs.d/programming")

;; Core packages
(message "Loading core packages")
(require 'package-setup)  ;; Package management setup
(message "Loading utils")
(require 'utils)          ;; Utility functions
(message "backup-setup")
(require 'backup-setup)   ;; Backup configuration
(message "loading editing")
(require 'editing)        ;; Editing configuration
(message "loading navigation")
(require 'navigation)     ;; Navigation and movements

;; UI configuration
(message "loading theme")
(require 'theme)          ;; Theme and visual settings

;; Programming
(message "loading shared")
(require 'shared)         ;; Shared programming utilities and tools

;; Language specific configurations
(message "loading python")
(require 'python-config)  ;; Python configuration
(message "loading swift")
(require 'swift)          ;; Swift configuration
(message "loading web")
(require 'web)            ;; Web development configuration

;; LSP hooks for different modes
(add-hook 'java-mode-hook 'my/lsp-deferred)
(add-hook 'web-mode-hook 'my/lsp-deferred)
(add-hook 'swift-mode-hook 'my/lsp-deferred)
(add-hook 'typescript-mode-hook 'my/lsp-deferred)
(add-hook 'tide-mode-hook 'my/lsp-deferred)
(add-hook 'python-mode-hook 'my/lsp-deferred)
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

(message "Emacs configuration loaded successfully")
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((python-shell-interpreter . ".venv/bin/python"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
