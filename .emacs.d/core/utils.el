;; Utility Functions

;; Rotate Windows
(defun rotate-windows () 
  "Rotate your windows" 
  (interactive) 
  (cond ((not (> (count-windows) 1)) 
         (message "You can't rotate a single window!")) 
        (t (setq i 1) 
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

;; Copy current filename
(defun copy-file-path (&optional @dir-path-only-p) 
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the file/dir cursor is on, or marked files.
If a buffer is not file and not dired, copy value of `default-directory' (which is usually dir when that buffer was created)
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
    (kill-new (if @dir-path-only-p 
                  (progn (message "Directory path copied: Currentthe%s " (file-name-directory $fpath)) 
                         (file-name-directory $fpath)) 
                (progn (message "File path copied: %s" $fpath) $fpath )))))

;; Terminal functions
(defun jnm/term-toggle-mode () 
  "Toggles term between line mode and char mode" 
  (interactive) 
  (if (term-in-line-mode) 
      (term-char-mode) 
    (term-line-mode))) 

(defun open-terminal-dot-app-here () 
  (interactive) 
  (shell-command "open -a Ghostty \"$pwd\"")) 

(defun open-term-here () 
  (interactive) 
  (let ((buf (term "/bin/zsh"))) 
    (switch-to-buffer (other-buffer buf)) 
    (switch-to-buffer-other-window buf)))

;; Toggle fold function
(defun toggle-fold () 
  (interactive) 
  (save-excursion 
    (end-of-line) 
    (hs-toggle-hiding)))

(provide 'utils)