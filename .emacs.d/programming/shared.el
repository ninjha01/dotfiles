;;; shared.el --- Shared programming configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration shared across multiple programming languages.

;;; Code:

;; Git integration (load before projectile)
(use-package magit

  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)
         ("C-c b" . magit-blame))
  :config
  (setq magit-save-repository-buffers 'dontask
        magit-diff-visit-prefer-worktree t
        magit-after-save-refresh-status t))

;; Projectile for project management
(use-package projectile
  :after magit
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init
  (projectile-mode 1)
  :config
  (setq projectile-indexing-method 'native
        projectile-sort-order 'recently-active
        projectile-git-submodule-command nil
        projectile-require-project-root t
        projectile-ignored-projects '("~/" "/opt/homebrew")
        projectile-switch-project-action 'magit-status)
  (dolist (dir '("Pods" ".next" "build" "straight" "node_modules" ".mypy_cache" ".venv" "__pycache__" ".ruff_cache" ".git"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (dolist (ext '(".pyc" ".DS_Store"))
    (add-to-list 'projectile-globally-ignored-file-suffixes ext)))

(use-package magit-todos

  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:"))

(use-package git-link

  :bind ("C-c l" . git-link))

;; LSP Mode configuration
(defun my/lsp-tramp-buffer-p ()
  "Return non-nil if the current buffer is a TRAMP buffer."
  (and (boundp 'tramp-file-name-regexp)
       buffer-file-name
       (string-match-p tramp-file-name-regexp buffer-file-name)))

(defun my/lsp-deferred ()
  "Call `lsp-deferred' unless in a TRAMP buffer."
  (unless (my/lsp-tramp-buffer-p)
    (lsp-deferred)))

(use-package lsp-mode

  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-<return>" . lsp-execute-code-action))
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-auto-guess-root t
        lsp-restart 'auto-restart
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-idle-delay 0.5
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-file-watchers nil
        lsp-modeline-code-actions-enable nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil))

;; LSP UI
(use-package lsp-ui

  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("C-c C-d" . lsp-ui-doc-show))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil))

;; Company mode for completions
(use-package company
  :defer t
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-box

  :hook (company-mode . company-box-mode))

;; Syntax checking
(use-package flycheck

  :init
  (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c e" . flycheck-next-error)
              ("C-c C-e" . flycheck-list-errors)))

;; Code snippets
(use-package yasnippet

  :config
  (yas-global-mode 1))

;; Markdown support
(use-package markdown-mode

  :mode "\\.md\\'"
  :hook (markdown-mode . (lambda ()
                           (flycheck-mode 1)
                           (company-mode -1)
                           (copilot-mode -1)
                           (olivetti-mode 1))))

;; Spelling
(use-package flycheck-aspell
  )

;; GitHub Copilot
(use-package copilot
  :straight (:host github
                   :repo "zerolfx/copilot.el"
                   :files ("dist" "*.el"))
  :defer t
  :init
  (setq copilot-node-executable "node")
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (with-eval-after-load 'company
    (delq 'company-preview-if-just-one-frontend company-frontends))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; AI integration
(use-package gptel
  :straight (:host github
                   :repo "karthink/gptel"
                   :files ("*.el"))

  :bind ("C-c C-g" . gptel)
  :init
  (setq gptel-default-mode 'markdown-mode)
  (setq-default gptel-model "claude-opus-4-5"
                gptel-backend (gptel-make-anthropic "Claude"
                                :stream t
                                :key (getenv "ANTHROPIC_API_KEY")))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("codellama:34b"))
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-<return>") 'gptel-send)
    (define-key markdown-mode-map (kbd "C-c C-c") 'gptel-send)))

;; Other file formats
(use-package graphviz-dot-mode
  )

(use-package yaml-mode
  )

(use-package csv-mode
  )

(use-package dockerfile-mode
  )

(use-package terraform-mode
  )

(provide 'shared)
