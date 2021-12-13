(setq style-package-list '(rainbow-delimiters doom-modeline doom-themes fira-code-mode))
(dolist (package style-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

;; Smooth scrolling
(pixel-scroll-mode -1)

;; Don't show gaps on macOS with no titlebar
(setq frame-resize-pixelwise t)

;; Rainbow delimiters
(rainbow-delimiters-mode-enable)

;; emoji
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))


;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

;; remove fringe color
;; https://emacs.stackexchange.com/questions/5342/how-do-i-set-the-fringe-colors-to-whatever-is-the-background-color
(set-face-attribute 'fringe nil :background nil)

(setq default-frame-alist '((font . "Fira Code 12")))

;; (straight-use-package
;;  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
;; (require 'nano-base-colors)
;; (require 'nano-faces)
;; (require 'nano-layout)
;; (require 'nano-theme-dark)
;; (require 'nano-theme)

(load-theme 'doom-zenburn t)

(doom-modeline-mode t)
(set-face-attribute 'mode-line nil :font "Fira Code 12")
(setq doom-modeline-height 22)
(setq doom-modeline-icon nil)
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

(setq inhibit-startup-screen t)

;; Highlighted regions are grey with white text
(set-face-attribute 'region nil 
		    :background "#666" 
		    :foreground "#ffffff")





;;; Remove menubar
(menu-bar-mode -1)
;;; Remove Toolbar
(tool-bar-mode -1)
;;; Remove scroll bar
(scroll-bar-mode -1)

;;; Delimiters
(show-paren-mode)

;;Clear Scratch
(setq initial-scratch-message "")

;; EWW
;; Hide all images
(setq shr-inhibit-images t)
;; set background color to servicable gray
(setq shr-color-visible-luminance-min 70)
