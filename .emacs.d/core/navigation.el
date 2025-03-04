;; Navigation and Editor Enhancement

;; Window management
(use-package ace-window 
  :ensure t 
  :bind (:map global-map ("C-x o" . ace-window)))

;; Window splitting shortcuts
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-2") 'split-window-below)
(save-place-mode 1)

;; Multiple cursors
(use-package multiple-cursors 
  :ensure t 
  :bind (:map global-map 
              ("C->" . mc/mark-next-like-this) 
              ("C-<" . mc/mark-previous-like-this)))

;; Subword navigation
(use-package subword)
(global-subword-mode)

;; Code folding
;; Enable hs-minor-mode for programming modes
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; Globally bind the key for toggling code folding
(global-set-key (kbd "<backtab>") 'toggle-fold)
(global-set-key (kbd "C-c <tab>") 'hs-show-all)
(global-set-key (kbd "C-c <backtab>") 'hs-hide-all)

;; Which-key for keybinding help
(use-package which-key 
  :ensure t)

(which-key-mode 1)

;; Symbol prettification
(global-prettify-symbols-mode t)

;; Mac specific key remappings
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; Rotate windows
(global-set-key (kbd "C-S-r") 'rotate-windows)

;; Query regexp replace
(global-set-key (kbd "C-c r") 'query-replace-regexp)

;; Move text up and down
(use-package move-text 
  :ensure t 
  :bind (:map global-map 
              ("M-S-<up>" . move-text-up) 
              ("M-S-<down>" . move-text-down)))

;; Cycle spacing
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Avy for quick navigation
(use-package avy 
  :bind (:map global-map ("M-s" . 'avy-goto-char-timer)))

;; Crux utilities
(use-package crux 
  :ensure t 
  :bind (:map global-map 
              ("C-x C-r" . crux-rename-file-and-buffer) 
              ("C-x C-k" . crux-delete-buffer-and-file)))

;; Ivy, Counsel, Swiper for completion
(use-package ivy 
  :ensure t 
  :config 
  (setq ivy-use-virtual-buffers t) 
  (setq enable-recursive-minibuffers t) 
  :bind (:map global-map 
              ("C-s" . swiper) 
              ("C-c C-r" . ivy-resume) 
              ("C-x b" . ivy-switch-buffer)))

(ivy-mode)

(use-package counsel 
  :ensure t 
  :bind (:map global-map 
              ("M-x" . counsel-M-x) 
              ("C-x C-f" . counsel-find-file) 
              ("C-c k" . counsel-git-grep) 
              ("C-r" . counsel-minibuffer-history)))

;; Don't jump around on scroll
(setq scroll-conservatively 100)

;; Split window sizing thresholds
(setq split-height-threshold 50    ; lines to place window below
      split-width-threshold 200)   ; cols to place window to the right

;; Writable grep
(use-package wgrep 
  :ensure t)

;; Command logging for demonstrations
(use-package command-log-mode 
  :ensure t)

;; Persistent scratch buffer
(use-package persistent-scratch 
  :ensure t)

(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1)

;; Centered and focused editing
(use-package centered-cursor-mode 
  :ensure t)

(use-package olivetti 
  :ensure t)

(provide 'navigation)