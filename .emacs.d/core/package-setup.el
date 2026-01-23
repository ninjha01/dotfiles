;;; package-setup.el --- Package management setup -*- lexical-binding: t -*-

;;; Commentary:
;; Bootstrap straight.el and use-package for package management.

;;; Code:

(add-to-list 'load-path "~/.emacs.d/elisp")

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Skip repo checks in CI environments
(when (string-equal (getenv "SKIP_REPOS_DOWNLOAD") "true")
  (setq straight-check-for-modifications nil))

;; Configure straight.el (must be set before loading use-package)
(setq straight-use-package-by-default t
      straight-vc-git-default-protocol 'ssh)

;; Install use-package
(straight-use-package 'use-package)

(defun package-setup--process-run-error-handler (func &rest args)
  "Advice for FUNC to display process buffer on error.
Calls FUNC with ARGS and shows `straight-process-buffer' on failure."
  (condition-case err
      (apply func args)
    (error
     (switch-to-buffer straight-process-buffer)
     (signal (car err) (cdr err)))))

(advice-add 'straight--process-run
            :around #'package-setup--process-run-error-handler)

;; Suppress native compilation warnings
(setq warning-minimum-level :error)

;; Add pnpm to exec-path for copilot language server
(defconst package-setup--pnpm-path (expand-file-name "~/Library/pnpm")
  "Path to pnpm installation directory.")

(add-to-list 'exec-path package-setup--pnpm-path)
(setenv "PATH" (concat (getenv "PATH") ":" package-setup--pnpm-path))

(provide 'package-setup)
;;; package-setup.el ends here
