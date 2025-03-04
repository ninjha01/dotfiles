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
  (defvar prettier-config-frontend t "Toggle between frontend and root package.json for prettier config.") 
  
  (defun toggle-prettier-config-path () 
    "Toggle prettier config path between frontend and root package.json." 
    (interactive) 
    (setq prettier-config-frontend (not prettier-config-frontend)) 
    (setf (alist-get 'prettier apheleia-formatters) 
          `(npx "prettier" "--config" ,(if prettier-config-frontend 
                                           (concat (projectile-project-root) "frontend/package.json") 
                                         (concat (projectile-project-root) "package.json")) 
                "--stdin-filepath" filepath)) 
    (message "Prettier config path set to %s" 
             (if prettier-config-frontend "frontend/package.json" "package.json"))) 
  
  (setf (alist-get 'prettier apheleia-formatters) 
        '(npx "prettier" "--config" (concat (projectile-project-root) "frontend/package.json") 
              "--stdin-filepath" filepath)) 
  
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier)) 
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