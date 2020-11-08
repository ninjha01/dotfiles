;; Package system
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)


(setq package-list '(emojify persistent-scratch ace-window beacon  browse-kill-ring dumb-jump
			     elisp-format  fireplace  flyparens god-mode helm-flycheck htmlize
			     ledger-mode markdown-mode mood-line multiple-cursors  pdf-tools
			     rainbow-delimiters  todoist use-package vlf  which-key))
(dolist (package package-list) 
  (unless (package-installed-p package) 
    (package-install package)))


;; General
(require 'init-magit)
(require 'init-keyfreq)
(require 'init-projectile)
(require 'init-org)
(require 'init-company)
(require 'init-shell)
(require 'init-flycheck)
(require 'init-web)
(require 'init-yaml)
(require 'init-docker)
(require 'init-clisp)
(require 'init-rust)
(require 'init-clang)
(require 'init-java)
(require 'conseq-mode)


;; Rainbow delimiters
(rainbow-delimiters-mode-enable)

;; emoji
(emojify-mode)
(emojify-set-emoji-styles '(unicode))

;; subword mode
(global-subword-mode 1)


;;; Browse Kill ring
(global-set-key (kbd "C-c y") 'browse-kill-ring)


					;:; God mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)


;;; Ivy
(require 'init-ivy)

;;; Save Place Mode
(save-place-mode 1)


;; Ace Window
(global-set-key (kbd "C-x o") 'ace-window)


;; move with S-r, S-l, etc.
(windmove-default-keybindings 'super)


;; Window manipulation undo tree
(winner-mode 1)


;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


;;; Subword-mode CamelCase handling
(require 'subword)
(subword-mode 1)


;;; Which-key
(require 'which-key)
(which-key-mode 1)


(require 'vlf-setup)


;; persistent scratch
(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1)
;;; Rainbow delimters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;; global hl line mode
(global-hl-line-mode t)
;;; global prettify symbols mode
(global-prettify-symbols-mode t)
;;; beacon mode
(beacon-mode t)
;;; mood line
(mood-line-mode)




;;; Fuzzy Matching
;;; TODO: Is this unneccasrry w/ counsel?
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;;; Go to home dir when typing ~ in file completion
(add-hook 'ido-setup-hook 
	  (lambda ()
	    ;; Go straight home
	    (define-key ido-file-completion-map (kbd "~") 
	      (lambda () 
		(interactive) 
		(if (looking-back "/") 
		    (insert "~/") 
		  (call-interactively 'self-insert-command))))))
