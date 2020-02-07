;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                Nishant's Emacs Configuration                  ;;
;;                    There are many like it                     ;;
;;                     But this one is mine                      ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "style")
(load-library "modes")
(load-library "keys")

;; Write backup files to own directory
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/emacs-saves/" t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Don't jump around on scroll
(setq scroll-conservatively 100)

;; Emacs first checks whether a window can be split horizontally when opening a new window, then checks if it can be split vertically.
;; This function reverses the order of the checks.
(defun reversed-split-window-sensibly
    (&optional
     window)
  (let ((window (or window
		    (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window (split-window-right)))
	(and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window (split-window-below)))
	(and (eq window (frame-root-window (window-frame window)))
	     (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
	       (when (window-splittable-p window t)
		 (with-selected-window window (split-window-right))))))))

(setq split-window-preferred-function 'reversed-split-window-sensibly)


;; macOS shell tomfoolery
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string
								   "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setq shell-command-switch "-ic")
(put 'downcase-region 'disabled nil)

;; track commands on emacs kill
(add-hook `kill-emacs-hook
	  (lambda ()
	    (f-append-text (format "<START SESSION %s>\n" (current-time-string)) 'utf-8
			   "~/.emacs.d/commands")
	    (f-append-text (format "%s" command-history) 'utf-8 "~/.emacs.d/commands")
	    (f-append-text (format "\n<END SESSION %s>\n" (current-time-string)) 'utf-8
			   "~/.emacs.d/commands")))
