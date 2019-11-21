;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                Nishant's Emacs Configuration                  ;;
;;                    There are many like it                     ;;
;;                     But this one is mine                      ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/")
(load-library "style")
(load-library "modes")
(load-library "keys")

;; Write backup files to own directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs-saves/" t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Don't jump around on scroll
(setq scroll-conservatively 100)

;; macOS shell tomfoolery
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setq shell-command-switch "-ic")
(put 'downcase-region 'disabled nil)
