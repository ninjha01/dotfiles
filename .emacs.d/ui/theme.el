;;; theme.el --- Theme and UI setup -*- lexical-binding: t -*-

;;; Commentary:
;; Visual appearance configuration including theme, modeline, and chrome.

;;; Code:

;; Theme
(use-package zenburn-theme

  :config
  (load-theme 'zenburn t))

;; Line numbers (displayed temporarily for goto-line)
(use-package display-line-numbers
  :commands display-line-numbers-mode
  :config
  (set-face-foreground 'line-number "#7F9F7F")
  :init
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily while prompting for line number."
    (interactive)
    (unwind-protect
        (progn
          (display-line-numbers-mode 1)
          (goto-line (read-number "Goto line: ")))
      (display-line-numbers-mode -1))))

;; Modeline
(use-package shrink-path

  :straight (:host github
                   :repo "zbelial/shrink-path.el"
                   :files ("dist" "*.el")))

(use-package all-the-icons
  )

(use-package doom-modeline

  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 22
        doom-modeline-bar-width 1
        doom-modeline-major-mode-color-icon t
        doom-modeline-env-version nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-buffer-modification-icon nil
        doom-modeline-checker-simple-format t
        doom-modeline-indent-info nil
        doom-modeline-minor-modes nil
        doom-modeline-project-detection 'projectile
        doom-modeline-vcs-max-length 12))

;; Chrome - remove UI clutter
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'fringe nil :background nil)
(setq frame-resize-pixelwise t)

;; Delimiters
(use-package rainbow-delimiters
  :defer t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(show-paren-mode 1)

;; Startup behavior
(setq inhibit-startup-screen t
      initial-scratch-message "")

;; Selection highlighting
(set-face-attribute 'region nil
                    :background "#666"
                    :foreground "#ffffff")

;; Highlight current line
(global-hl-line-mode 1)

;; Improved cursor location visibility
(use-package beacon

  :config
  (beacon-mode 1))

(provide 'theme)
