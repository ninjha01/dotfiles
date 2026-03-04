;; Backup Configuration

;; Write backup files to own directory
(unless (file-exists-p "~/.emacs.d/.saves/") 
  (make-directory "~/.emacs.d/.saves/"))

(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(unless (file-exists-p "~/.emacs.d/emacs-saves/") 
  (make-directory "~/.emacs.d/emacs-saves/"))

(defun hash-file-name (file-path) 
  (let ((hashed-name (sha1 file-path)))
    ;; Ensure the mapping directory exists
    (unless (file-exists-p "~/.emacs.d/emacs-saves/") 
      (make-directory "~/.emacs.d/emacs-saves/" t))
    ;; Ensure the hash file exists
    (unless (file-exists-p "~/.emacs.d/emacs-saves/file_to_hash_map.txt") 
      (with-temp-buffer (write-file "~/.emacs.d/emacs-saves/file_to_hash_map.txt" t))) 
    (with-temp-file "~/.emacs.d/emacs-saves/file_to_hash_map.txt" 
      (insert-file-contents "~/.emacs.d/emacs-saves/file_to_hash_map.txt") 
      (goto-char (point-max)) 
      (insert (format "%s -> %s\n" file-path hashed-name))) 
    hashed-name))

;; sometimes the filename is too long, so we hash the filename and store a record to lookup the filename later
(setq auto-save-file-name-transforms `((".*" ,(concat "~/.emacs.d/emacs-saves/" (hash-file-name "\\1")) t)))
(setq auto-save-default t)
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Set trash directory
(setq trash-directory "~/.Trash")

(provide 'backup-setup)