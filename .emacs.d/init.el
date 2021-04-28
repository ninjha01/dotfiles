;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                Nishant's Emacs Configuration                  ;;
;;                    There are many like it                     ;;
;;                     But this one is mine                      ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(package-refresh-contents)
(load-library "style")
(load-library "modes")
(load-library "keys")

;; Write backup files to own directory
(unless (file-exists-p "~/.emacs.d/.saves/") 
  (make-directory "~/.emacs.d/.saves/"))
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

(unless (file-exists-p "~/.emacs.d/emacs-saves/") 
  (make-directory "~/.emacs.d/emacs-saves/"))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/emacs-saves/" t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Don't jump around on scroll
(setq scroll-conservatively 100)


;; Region highlights as white
(set-face-attribute 'region nil 
		    :background "#666" 
		    :foreground "#ffffff")

;; Emacs first checks whether a window can be split horizontally when opening a new window, then checks if it can be split vertically.
;; This function reverses the order of the checks.
(defun reversed-split-window-sensibly 
    (&optional 
     window
     ) 
  (let ((window (or window 
		    (selected-window)
		    ))) 
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window (split-window-right))
	     ) 
	(and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window (split-window-below))
	     ) 
	(and (eq window (frame-root-window (window-frame window))) 
	     (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0)) 
	       (when (window-splittable-p window t) 
		 (with-selected-window window (split-window-right))))
	     )
	))
  )

(setq split-window-preferred-function 'reversed-split-window-sensibly)


;; macOS shell tomfoolery
(defun set-exec-path-from-shell-PATH () 
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string
								   "$SHELL --login -i -c 'echo $PATH'")))) 
    (setenv "PATH" path-from-shell) 
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator)))
  )

(when window-system (set-exec-path-from-shell-PATH))
(setq shell-command-switch "-ic")
(setq-default explicit-shell-file-name "/bin/bash")
(put 'downcase-region 'disabled nil)

;; track all history
(setq history-length t)
;; track commands on emacs kill
(add-hook `kill-emacs-hook 
	  (lambda () 
	    (append-to-file (format "<START SESSION %s>\n" (current-time-string)) 'utf-8 "~/.emacs.d/commands") 
	    (append-to-file (format "%s" command-history) 'utf-8 "~/.emacs.d/commands") 
	    (append-to-file (format "\n<END SESSION %s>\n" (current-time-string)) 'utf-8 "~/.emacs.d/commands")
	    ))

;; Add logging for when emacs hangs
(setq-default garbage-collection-messages t)
;; Copy current filename
(defun copy-file-path 
    (&optional 
     @dir-path-only-p
     )
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2017-09-01" 
  (interactive "P") 
  (let (($fpath (if (string-equal major-mode 'dired-mode) 
		    (progn (let (($result (mapconcat 'identity (dired-get-marked-files) "\n"))) 
			     (if (equal (length $result) 0) 
				 (progn default-directory ) 
			       (progn $result)))) 
		  (if (buffer-file-name) 
		      (buffer-file-name) 
		    (expand-file-name default-directory))))) 
    (kill-new (if @dir-path-only-p (progn (message "Directory path copied: 「%s」" (file-name-directory $fpath)) 
					  (file-name-directory $fpath)) 
		(progn (message "File path copied: 「%s」" $fpath) $fpath ))))
  )

(setq trash-directory "~/.Trash")

(defun set-selective-display-dlw (&optional level)
"Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0.
https://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs"
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))
(global-set-key (kbd "C-x C-4") 'set-selective-display-dlw)


(setq split-height-threshold 260
      split-width-threshold 260)
