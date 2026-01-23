;;; editing.el --- Editing configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Settings for editing behavior, terminals, and code formatting.

;;; Code:

;;; Performance settings
(setq gc-cons-threshold (* 100 1024 1024)  ; 100MB
      read-process-output-max (* 1024 1024))
(electric-indent-mode -1)

;; Disable auto-save and backups
(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

;; Log garbage collection for debugging hangs
(setq-default garbage-collection-messages t)

;;; Terminal
(use-package term
  :bind (("s-t" . nj/open-terminal-app)
         ("C-c t" . nj/open-term-here)
         :map term-mode-map
         ("C-c C-j" . nj/term-toggle-mode)))

;;; Shell mode
(defun editing--shell-mode-setup ()
  "Configure shell mode with minimal font-lock and buffer truncation."
  (font-lock-mode -1)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil t))

(use-package shell
  :ensure nil
  :hook (shell-mode . editing--shell-mode-setup)
  :config
  (setq comint-buffer-maximum-size 5000))

;;; Formatting

(defconst editing--prettier-config-files
  '(".prettierrc" ".prettierrc.json" ".prettierrc.js"
    "prettier.config.js" "package.json")
  "List of Prettier configuration file names.")

(defun editing--find-prettier-config ()
  "Find nearest Prettier config file from current buffer's directory."
  (when-let ((dir (and buffer-file-name
                       (file-name-directory buffer-file-name))))
    (seq-some (lambda (file) (locate-dominating-file dir file))
              editing--prettier-config-files)))

(defun editing--biome-config-p (dir)
  "Return non-nil if DIR has a Biome configuration file."
  (or (locate-dominating-file dir "biome.json")
      (locate-dominating-file dir "biome.jsonc")))

(defun editing--set-js-formatter ()
  "Set formatter to Biome if config exists, otherwise Prettier."
  (when-let ((dir (and buffer-file-name
                       (file-name-directory buffer-file-name))))
    (setq-local apheleia-formatter
                (if (editing--biome-config-p dir) 'biome 'prettier))))

(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--config" (or (editing--find-prettier-config) (projectile-project-root))
              "--stdin-filepath" filepath))

  (setf (alist-get 'biome apheleia-formatters)
        '("pnpm" "dlx" "@biomejs/biome" "format" "--stdin-file-path" filepath))

  (add-hook 'web-mode-hook #'editing--set-js-formatter)
  (add-hook 'typescript-mode-hook #'editing--set-js-formatter)
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
  (apheleia-global-mode t))

(use-package elisp-format)

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-buffer))

(provide 'editing)
;;; editing.el ends here
