
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper)


;; Open and name shell
(global-set-key (kbd "M-s M-s") 
		(lambda () 
		  (interactive) 
		  (shell) 
		  (rename-uniquely)))


;; Window Manipulation

;; Expand windows
(global-set-key (kbd "C-^") 'enlarge-window)
;;; (global-set-key (kbd "C-") 'shrink-window)
(global-set-key (kbd "C-,") 'shrink-window-horizontally)
(global-set-key (kbd "C-.") 'enlarge-window-horizontally)

;; Rotate Windows
(defun rotate-windows () 
  "Rotate your windows" 
  (interactive) 
  (cond ((not (> (count-windows) 1)) 
	 (message "You can't rotate a single window!")) 
	(t 
	 (setq i 1) 
	 (setq numWindows (count-windows)) 
	 (while  (< i numWindows) 
	   (let* ((w1 (elt (window-list) i)) 
		  (w2 (elt (window-list) 
			   (+ (% i numWindows) 1)))
		  (b1 (window-buffer w1)) 
		  (b2 (window-buffer w2))
		  (s1 (window-start w1)) 
		  (s2 (window-start w2))) 
	     (set-window-buffer w1  b2) 
	     (set-window-buffer w2 b1) 
	     (set-window-start w1 s2) 
	     (set-window-start w2 s1) 
	     (setq i (1+ i)))))))
(global-set-key (kbd "C-S-r") 'rotate-windows)


(defun delete-current-buffer-file () 
  "Removes file connected to current buffer and kills buffer." 
  (interactive) 
  (let ((filename (buffer-file-name)) 
	(buffer (current-buffer)) 
	(name (buffer-name))) 
    (if (not (and filename 
		  (file-exists-p filename))) 
	(ido-kill-buffer) 
      (when (yes-or-no-p "Are you sure you want to remove this file? ") 
	(delete-file filename) 
	(kill-buffer buffer) 
	(message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)


(defun rename-current-buffer-file () 
  "Renames current buffer and file it is visiting." 
  (interactive) 
  (let ((name (buffer-name)) 
	(filename (buffer-file-name))) 
    (if (not (and filename 
		  (file-exists-p filename))) 
	(error 
	 "Buffer '%s' is not visiting a file!"
	 name) 
      (let ((new-name (read-file-name "New name: " filename))) 
	(if (get-buffer new-name) 
	    (error 
	     "A buffer named '%s' already exists!"
	     new-name) 
	  (rename-file filename new-name 1) 
	  (rename-buffer new-name) 
	  (set-visited-file-name new-name) 
	  (set-buffer-modified-p nil) 
	  (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory
								  new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)


;; Movement

;; Rearrange lines up and down
(defun move-line-down () 
  (interactive) 
  (let ((col (current-column))) 
    (save-excursion (forward-line) 
		    (transpose-lines 1)) 
    (forward-line) 
    (move-to-column col)))

(global-set-key (kbd "C-S-<down>") 'move-line-down)


(defun move-line-up () 
  (interactive) 
  (let ((col (current-column))) 
    (save-excursion (forward-line) 
		    (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-S-<up>") 'move-line-up)


(defun goto-line-with-feedback () 
  "Show line numbers temporarily, while prompting for the line number input" 
  (interactive) 
  (unwind-protect (progn (linum-mode 1) 
			 (goto-line (read-number "Goto line: "))) 
    (linum-mode -1)))

(global-set-key (kbd "M-l") 'goto-line-with-feedback)


;; Opening newlines
(defun open-line-below () 
  (interactive) 
  (end-of-line) 
  (newline) 
  (indent-for-tab-command))

(global-set-key (kbd "C-RET") 'open-line-below)


(defun open-line-above () 
  (interactive) 
  (beginning-of-line) 
  (newline) 
  (forward-line -1) 
  (indent-for-tab-command))

(global-set-key (kbd "C-s-RET") 'open-line-above)

;; Open Terminal to cur dir with CMD-T
(defun open-term-here ()
  (interactive)
  (shell-command "open -a Terminal \"$pwd\""))

(global-set-key (kbd "s-t") 'open-term-here)


;; Query regexp replace
(global-set-key (kbd "C-c r") 'query-replace-regexp)
