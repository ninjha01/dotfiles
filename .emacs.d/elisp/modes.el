(use-package 
  persistent-scratch 
  :ensure t)

(use-package 
  ace-window 
  :ensure t)

(use-package 
  beacon 
  :ensure t)

(use-package 
  browse-kill-ring 
  :ensure t)

(use-package 
  dumb-jump 
  :ensure t)

(use-package 
  elisp-format 
  :ensure t)

(use-package 
  fireplace 
  :ensure t)


(use-package 
  god-mode 
  :ensure t)

(use-package 
  helm-flycheck 
  :ensure t)

(use-package 
  htmlize 
  :ensure t)

(use-package 
  markdown-mode 
  :ensure t)

(use-package 
  mood-line 
  :ensure t)

(use-package 
  multiple-cursors 
  :ensure t)

(use-package 
  pdf-tools 
  :ensure t)

(use-package 
  which-key 
  :ensure t)

(use-package 
  atomic-chrome 
  :ensure t)

;; Tools
(require 'init-git) 			; use-packaged
(require 'init-docker)			; use-packaged
(require 'init-org)			; use-packaged
(require 'init-shell)			; use-packaged

;; General
(require 'init-projectile)		; use-packaged
(require 'init-keyfreq)			; use-packaged
(require 'init-company)
(require 'init-ivy)
(require 'init-crux)
(require 'init-avy)

;; Languages
(require 'init-lsp)
(require 'init-flycheck)
(require 'init-python)
(require 'init-clang)
(require 'init-clisp)
(require 'init-graphviz)
(require 'init-rust)
(require 'init-yaml)
(require 'init-sql)
;; (require 'init-ocaml)



;; Use emacs to edit text in chrome
(atomic-chrome-start-server)

;; subword mode
(global-subword-mode 1)


;;; Browse Kill ring
(global-set-key (kbd "C-c y") 'browse-kill-ring)


;;; God mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)




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
(add-hook 'ido-setup-hook (lambda ()
			    ;; Go straight home
			    (define-key ido-file-completion-map (kbd "~") 
			      (lambda () 
				(interactive) 
				(if (looking-back "/") 
				    (insert "~/") 
				  (call-interactively 'self-insert-command))))))
