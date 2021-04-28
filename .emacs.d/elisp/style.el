(setq inhibit-startup-screen t)
(set-face-attribute 'region nil 
		    :background "#666" 
		    :foreground "#ffffff")
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
