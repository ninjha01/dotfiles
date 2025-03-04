;; Theme and UI Setup

;; Theme
(use-package zenburn-theme 
  :ensure t 
  :config (load-theme 'zenburn t))

;; Line numbers
(use-package display-line-numbers 
  :commands (display-line-numbers-mode) 
  :config (set-face-foreground 'line-number "#7F9F7F") 
  :init (defun goto-line-with-feedback () 
          "Show line numbers temporarily, while prompting for the line number input" 
          (interactive) 
          (unwind-protect 
              (progn 
                (display-line-numbers-mode 1) 
                (goto-line (read-number "Goto line: "))) 
            (display-line-numbers-mode -1))) 
  (global-set-key (kbd "M-l") 'goto-line-with-feedback))

;; Modeline
(use-package shrink-path 
  :ensure t 
  :straight (:host github 
                   :repo "zbelial/shrink-path.el" 
                   :files ("dist" "*.el")))

(use-package all-the-icons 
  :ensure t)

(use-package doom-modeline 
  :ensure t 
  :init (doom-modeline-mode t) 
  :config 
  (setq dnoom-modeline-height 22) 
  (setq doom-modeline-major-mode-color-icon t) 
  (setq doom-modeline-env-version nil) 
  (setq doom-modeline-bar-width 1) 
  (setq doom-modeline-buffer-encoding nil) 
  (setq doom-modeline-buffer-file-name-style 'auto) 
  (setq doom-modeline-buffer-modification-icon nil) 
  (setq doom-modeline-checker-simple-format t) 
  (setq doom-modeline-indent-info nil) 
  (setq doom-modeline-minor-modes nil) 
  (setq doom-modeline-project-detection 'projectile) 
  (setq doom-modeline-vcs-max-length 12) 
  (set-face-attribute 'mode-line nil 
                      :font "Fira Code 12"))

;; Font
(use-package fira-code-mode 
  :ensure t 
  :init (fira-code-mode) 
  :config
  ;; (fira-code-mode-install-fonts t) ;; Install if you haven't already
  (setq default-frame-alist '((font . "Fira Code 12"))))

;; Chrome
;;; Remove menubar
(menu-bar-mode -1)
;;; Remove Toolbar
(tool-bar-mode -1)
;;; Remove scroll bar
(scroll-bar-mode -1)

;; remove fringe color
;; https://emacs.stackexchange.com/questions/5342/how-do-i-set-the-fringe-colors-to-whatever-is-the-background-color
(set-face-attribute 'fringe nil 
                    :background nil)

;; Don't show gaps on resize
(setq frame-resize-pixelwise t)

;; Delimiters
(show-paren-mode)
(use-package rainbow-delimiters 
  :ensure t)

(rainbow-delimiters-mode)
(show-paren-mode)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Clear Scratch
(setq initial-scratch-message "")

;; Highlighted regions are grey with white text
(set-face-attribute 'region nil 
                    :background "#666" 
                    :foreground "#ffffff")

;; Highlight current line
(global-hl-line-mode t)

;; Improved cursor location visibility
(use-package beacon 
  :ensure t)

(beacon-mode t)

(provide 'theme)