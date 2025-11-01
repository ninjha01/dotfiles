;; Shared programming configuration used across multiple languages

;; Git integration (load before projectile)
(use-package magit
  :ensure t
  :bind (:map global-map
              ("C-x g" . magit-status)
              ("C-c g" . magit-file-dispatch)
              ("C-c b" . magit-blame))
  :config
  (setq magit-save-repository-buffers 'dontask)
  :hook (after-save-hook . (lambda ()
                             (setq magit-after-save-refresh-status t))))

;; Projectile for project management
(use-package projectile
  :ensure t
  :after magit
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-indexing-method 'native)
  (add-to-list 'projectile-globally-ignored-directories "Pods")
  (add-to-list 'projectile-globally-ignored-directories ".next")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "straight")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".mypy_cache")
  (setq projectile-git-submodule-command nil)
  (setq projectile-require-project-root t)
  (setq projectile-ignored-projects '("~/" "/opt/homebrew"))
  (setq projectile-switch-project-action 'magit-status))

(projectile-mode 1)

(use-package magit-todos 
  :ensure t 
  :config 
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:"))

(use-package git-link 
  :ensure t 
  :bind (:map global-map 
              ("C-c l" . git-link)))

;; LSP Mode configuration
(defun my/lsp-mode-should-not-activate () 
  "Return non-nil if the current buffer should not activate lsp-mode." 
  (and (boundp 'tramp-file-name-regexp) 
       buffer-file-name 
       (string-match-p tramp-file-name-regexp buffer-file-name)))

(defun my/lsp-deferred () 
  "Call `lsp-deferred` if the current buffer is not a TRAMP buffer." 
  (unless (my/lsp-mode-should-not-activate) 
    (lsp-deferred)))

(use-package lsp-mode 
  :ensure t 
  :init (setq lsp-keymap-prefix "C-c l") 
  :commands (lsp lsp-deferred) 
  :bind (:map lsp-mode-map 
              ("C-<return>" . lsp-execute-code-action)) 
  :config 
  (lsp-enable-which-key-integration t) 
  (setq lsp-auto-guess-root t) 
  (setq lsp-restart 'auto-restart) 
  (setq lsp-enable-symbol-highlighting nil) 
  (setq lsp-enable-on-type-formatting nil) 
  (setq lsp-idle-delay 0.5) 
  (setq lsp-headerline-breadcrumb-enable nil) 
  (setq lsp-enable-file-watchers nil)
  ;; Reduce the amount of clutter and verbosity
  (setq lsp-modeline-code-actions-enable nil 
        lsp-eldoc-enable-hover nil 
        lsp-signature-auto-activate nil))

;; LSP UI
(use-package lsp-ui 
  :ensure t 
  :commands lsp-ui-mode 
  :config
  ;; Disable automatic popups
  (setq lsp-ui-sideline-enable nil 
        lsp-ui-doc-enable nil)
  ;; Add a keybinding for showing documentation
  (define-key lsp-ui-mode-map (kbd "C-c C-d") 'lsp-ui-doc-show))

;; Company mode for completions
(use-package company 
  :ensure t 
  :hook (prog-mode . company-mode) 
  :bind (:map company-active-map 
              ("<tab>" . company-complete-selection) 
              ("C-n" . company-select-next) 
              ("C-p" . company-select-next))
  :config 
  (setq company-tooltip-align-annotations t) 
  :custom 
  (company-minimum-prefix-length 1) 
  (company-idle-delay 0.1))

(use-package company-box 
  :hook (company-mode . company-box-mode))

;; Syntax checking
(use-package flycheck 
  :ensure t 
  :init (global-flycheck-mode) 
  :bind (:map flycheck-mode-map 
              ("C-c e" . flycheck-next-error) 
              ("C-c C-e" . 'flycheck-list-errors)))

;; Code snippets
(use-package yasnippet 
  :ensure t 
  :config
  ;; Use yasnippet for code templates
  (yas-global-mode))

;; Markdown support
(use-package markdown-mode 
  :ensure t 
  :mode (".md") 
  :hook ((markdown-mode . (lambda () 
                            (flycheck-mode 1) 
                            (company-mode -1) ; Disable company-mode
                            (copilot-mode -1) ; Disable copilot-mode
                            (olivetti-mode 1) ; Enable olivetti-mode
                            ))))

;; Spelling
(use-package flycheck-aspell 
  :ensure t)

;; GitHub Copilot
(use-package copilot
  :straight (:host github
                   :repo "zerolfx/copilot.el"
                   :files ("dist" "*.el"))
  :ensure t
  :init
  ;; Set the path to the copilot language server
  (setq copilot-node-executable "node")
  :config
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  :hook ((prog-mode . copilot-mode)))

;; AI integration
(use-package gptel 
  :straight (:host github 
                   :repo "karthink/gptel" 
                   :files ("*.el")) 
  :ensure t 
  :bind (:map global-map 
              ("C-c C-g" . gptel)) 
  :init
  ;; (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-default-mode 'markdown-mode) 
  (setq-default gptel-model "claude-opus-4-20250514" 
                gptel-backend (gptel-make-anthropic "Claude" 
                                :stream t 
                                :key (getenv "ANTHROPIC_API_KEY"))) 
  (gptel-make-ollama "Ollama" 
    :host "localhost:11434" 
    :stream t 
    :models '("codellama:34b")) 
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response) 
  (require 'markdown-mode) 
  (define-key markdown-mode-map (kbd "C-<return>") 'gptel-send) 
  (define-key markdown-mode-map (kbd "C-c C-c") 'gptel-send))

;; Other file formats
(use-package graphviz-dot-mode 
  :ensure t)

(use-package yaml-mode 
  :ensure t)

(use-package csv-mode 
  :ensure t)

(use-package dockerfile-mode 
  :ensure t)

(use-package terraform-mode 
  :ensure t)

(provide 'shared)
