(setq lsp-package-list '(lsp-java lsp-mode lsp-ui))
(dolist (package lsp-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

;;; LSP mode
;; https://github.com/emacs-lsp/lsp-mode
;; "Adjust gc-cons-threshold. The default setting is too low for lsp-mode’s needs due to the fact that client/server communication generates a lot of memory/garbage."
(setq gc-cons-threshold 100000000)
;; "Increase the amount of data which Emacs reads from the process. Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range."
(setq read-process-output-max (* 1024 1024))
(require 'company)
(push 'company-lsp company-backends)

(provide 'init-lsp)
