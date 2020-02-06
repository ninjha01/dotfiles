(setq inhibit-startup-screen t)
(set-face-attribute 'region nil 
		    :background "#666" 
		    :foreground "#ffffff")
;;; Remove Toolbar
(tool-bar-mode -1)
;;; Remove scroll bar
(scroll-bar-mode -1)


;;Clear Scratch
(setq initial-scratch-message "")

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn))) 
 '(custom-safe-themes (quote ("acaccddbc0ae7d5c2cdea2e64b0261ca383671205752c062c44590d944ad0842"
			      default))) 
 '(package-selected-packages (quote (prettier-js tide js-comint flycheck-rust beacon ace-window
						 magit magit-topgit yaml-mode which-key web-mode vlf
						 use-package smooth-scroll rust-mode
						 multiple-cursors markdown-mode helm-flycheck
						 flyparens company-web company-shell company-go))) 
 '(vlf-application (quote dont-ask)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; EWW
;; Hide all images
(setq shr-inhibit-images t)
;; set background color to servicable gray
(setq shr-color-visible-luminance-min 70)
