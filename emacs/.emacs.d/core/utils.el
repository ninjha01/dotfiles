;;; utils.el --- Utility functions -*- lexical-binding: t -*-

;;; Commentary:
;; Custom utility functions for window management, file operations, and terminals.

;;; Code:

(require 'cl-lib)

(defun nj/rotate-windows ()
  "Rotate buffers between windows in the current frame."
  (interactive)
  (let ((windows (window-list)))
    (if (length< windows 2)
        (message "Cannot rotate a single window")
      (let* ((first-buf (window-buffer (car windows)))
             (first-start (window-start (car windows))))
        (cl-loop for (w1 w2) on windows
                 while w2
                 do (progn
                      (set-window-buffer w1 (window-buffer w2))
                      (set-window-start w1 (window-start w2))))
        (let ((last-window (car (last windows))))
          (set-window-buffer last-window first-buf)
          (set-window-start last-window first-start))))))

(defun nj/copy-file-path (&optional dir-only-p)
  "Copy the current buffer's file path to the kill ring.
With prefix argument DIR-ONLY-P, copy only the directory path.
In Dired, copy the path of marked files or file at point."
  (interactive "P")
  (let* ((path (cond
                ((derived-mode-p 'dired-mode)
                 (or (mapconcat #'identity (dired-get-marked-files) "\n")
                     default-directory))
                (buffer-file-name)
                (t (expand-file-name default-directory))))
         (result (if dir-only-p (file-name-directory path) path))
         (label (if dir-only-p "Directory" "Path")))
    (kill-new result)
    (message "%s copied: %s" label result)))

(defun nj/term-toggle-mode ()
  "Toggle terminal between line mode and char mode."
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun nj/open-terminal-app ()
  "Open Ghostty terminal at current directory."
  (interactive)
  (shell-command "open -a Ghostty \"$pwd\""))

(defun nj/open-term-here ()
  "Open a terminal buffer in other window."
  (interactive)
  (let ((buf (term "/bin/zsh")))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(defun nj/toggle-fold ()
  "Toggle code folding at point."
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))

(defun nj/connect-to-vanheron-vm ()
  "Connect to Nishant's server via TRAMP."
  (interactive)
  (find-file "/ssh:ec2-user@vanheron-vm#22:/home/ec2-user/nishant/vhl"))

(provide 'utils)
;;; utils.el ends here
