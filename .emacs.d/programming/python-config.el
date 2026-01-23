;; Python Mode Configuration

;; Load the built-in python-mode
(require 'python)

;; Python LSP documentation
(defun my-lsp-describe-at-point () 
  "Show LSP documentation in the minibuffer, with Markdown and HTML formatting stripped." 
  (interactive) 
  (let ((response (lsp-request "textDocument/hover" (lsp--text-document-position-params)))) 
    (if response 
        (let ((contents-hash (gethash "contents" response))) 
          (if (and contents-hash (hash-table-p contents-hash)) 
              (let ((doc-string (gethash "value" contents-hash)))
                ;; Remove Markdown formatting and replace HTML entities
                (setq doc-string (replace-regexp-in-string "```python" "" doc-string)) ; remove code block beginnning
                (setq doc-string (replace-regexp-in-string "```" "" doc-string)) ; remove code block ending
                (setq doc-string (replace-regexp-in-string "---" "" doc-string)) ; remove horizontal rules
                (setq doc-string (replace-regexp-in-string "&nbsp;" " " doc-string)) ; replace &nbsp; with space
                ;; Display processed string
                (message "%s" (or doc-string "Documentation not available."))) 
            (message "Documentation not available."))) 
      (message "No response from LSP server."))))

;; Python environment management
(use-package pyenv 
  )

;; Python LSP
(use-package lsp-pyright 

  :hook (python-mode . (lambda () 
                         (require 'lsp-pyright) 
                         (lsp-deferred))) 
  :bind (:map lsp-mode-map ("C-c d" . my-lsp-describe-at-point)) 
  :init (when (executable-find "python3") 
          (setq lsp-pyright-python-executable-cmd "python3")))

;; Python linting with ruff
(require 'flycheck-ruff)

;; Python dependency management
(use-package poetry 

  :hook (python-mode . poetry-tracking-mode))



(provide 'python-config)
