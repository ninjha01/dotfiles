;; Package Management Setup

(add-to-list 'load-path "~/.emacs.d/elisp")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)) 
      (bootstrap-version 5)) 
  (unless (file-exists-p bootstrap-file) 
    (with-current-buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el" 'silent 'inhibit-cookies) 
      (goto-char (point-max)) 
      (eval-print-last-sexp))) 
  (load bootstrap-file nil 'nomessage))

(when (string-equal (getenv "SKIP_REPOS_DOWNLOAD") "true") 
  (setq straight-check-for-modifications nil))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight 
  :custom (straight-use-package-by-default t) 
  :config (setq straight-vc-git-default-protocol 'ssh))

(defun my-straight-process-run-error-handler (func &rest args) 
  "Catch error from `straight--process-run', print process buffer, and re-signal error." 
  (condition-case err 
      (apply func args) 
    (error (progn 
             (switch-to-buffer straight-process-buffer) 
             (error "Caught error: %s" err)))))

(advice-add 'straight--process-run 
            :around #'my-straight-process-run-error-handler)

;; Supress native comp warnings buffer
(setq warning-minimum-level :error)

(package-initialize)

(provide 'package-setup)