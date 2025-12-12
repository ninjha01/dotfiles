;; Editing Configuration

;; Performance settings
(setq electric-indent-mode nil)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Disable default auto-save & backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Add logging for when emacs hangs
(setq-default garbage-collection-messages t)

;; Terminal configuration
(use-package term
  :bind (:map term-mode-map 
              ("C-c C-j" . jnm/term-toggle-mode))
  (:map global-map 
        ("s-t" . open-terminal-dot-app-here)
        ("C-c t" . open-term-here)))

;; Shell mode configuration
(use-package shell 
  :ensure nil 
  :hook (shell-mode . (lambda ()
                        ;; Disable font-lock mode
                        (font-lock-mode -1)
                        ;; Add hook to truncate buffer
                        (add-hook 'comint-output-filter-functions 'comint-truncate-buffer t t))) 
  :config
  ;; Set maximum buffer size
  (setq comint-buffer-maximum-size 5000))

;; General formatting
(use-package apheleia
  :ensure t
  :config
  (defun find-prettier-config ()
    "Find nearest Prettier config file from current buffer."
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (or (locate-dominating-file dir ".prettierrc")
            (locate-dominating-file dir ".prettierrc.json")
            (locate-dominating-file dir ".prettierrc.js")
            (locate-dominating-file dir "prettier.config.js")
            (locate-dominating-file dir "package.json")))))

  (defun set-js-formatter ()
    "Set apheleia-formatter to biome if biome.json exists, otherwise prettier."
    (setq-local apheleia-formatter
                (if (and buffer-file-name
                         (or (locate-dominating-file (file-name-directory buffer-file-name) "biome.json")
                             (locate-dominating-file (file-name-directory buffer-file-name) "biome.jsonc")))
                    'biome
                  'prettier)))

  ;; Prettier formatter with auto-detected config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--config" (or (find-prettier-config) (projectile-project-root))
              "--stdin-filepath" filepath))

  ;; Biome formatter (via pnpm dlx for local installs)
  (setf (alist-get 'biome apheleia-formatters)
        '("pnpm" "dlx" "@biomejs/biome" "format" "--stdin-file-path" filepath))

  ;; Auto-detect formatter for JS/TS modes via hook
  (add-hook 'web-mode-hook #'set-js-formatter)
  (add-hook 'typescript-mode-hook #'set-js-formatter)
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
  (apheleia-global-mode t))

;; Emacs Lisp formatting
(use-package elisp-format 
  :ensure t)

;; Emacs Lisp mode configuration
(use-package emacs-lisp-mode 
  :straight nil 
  :bind (:map emacs-lisp-mode-map 
              ("C-c C-c" . eval-buffer)))

(provide 'editing)