;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                Nishant's Emacs Configuration                  ;;
;;                    There are many like it                     ;;
;;                     But this one is mine                      ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "style")
(package-refresh-contents)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "acaccddbc0ae7d5c2cdea2e64b0261ca383671205752c062c44590d944ad0842" default))
 '(doom-modeline-mode t)
 '(package-selected-packages
   '(doom-modeline ewal-doom-themes doom-themes org-tree-slide wgrep-ag csv-mode fira-code-mode plantuml-mode google-this git-link graphviz-dot-mode prettier-js tide js-comint flycheck-rust beacon ace-window magit magit-topgit yaml-mode which-key web-mode vlf use-package smooth-scroll rust-mode multiple-cursors markdown-mode helm-flycheck flyparens company-web company-shell company-go))
 '(vlf-application 'dont-ask))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
