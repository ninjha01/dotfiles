;;; navigation.el --- Navigation and editor enhancement -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for navigation, window management, and completion.

;;; Code:

;; Window management
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;; Window splitting shortcuts
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-S-r") 'rotate-windows)

;; Save cursor position
(save-place-mode 1)

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; Subword navigation (navigate CamelCase)
(use-package subword
  :config
  (global-subword-mode 1))

;; Code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<backtab>") 'toggle-fold)
(global-set-key (kbd "C-c <tab>") 'hs-show-all)
(global-set-key (kbd "C-c <backtab>") 'hs-hide-all)

;; Which-key for keybinding help
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; Symbol prettification
(global-prettify-symbols-mode t)

;; Mac specific key remappings
(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      ns-function-modifier 'hyper)

;; Query regexp replace
(global-set-key (kbd "C-c r") 'query-replace-regexp)

;; Move text up and down
(use-package move-text
  :ensure t
  :bind (("M-S-<up>" . move-text-up)
         ("M-S-<down>" . move-text-down)))

;; Cycle spacing
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Avy for quick navigation
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char-timer))

;; Crux utilities
(use-package crux
  :ensure t
  :bind (("C-x C-r" . crux-rename-file-and-buffer)
         ("C-x C-k" . crux-delete-buffer-and-file)))

;; Modern completion stack: Vertico + Consult + Orderless + Embark

;; Vertico - vertical completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-resize nil))

;; Savehist - persist minibuffer history (enables recency sorting)
(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring project-prefix-history-list)
        history-length 1000))

;; Recentf - track recent files
(use-package recentf
  :init
  (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))

;; Orderless - flexible matching (space-separated terms)
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult - search and navigation commands
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c k" . consult-ripgrep)
         ("C-c f" . consult-find)
         ("M-l" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-y" . consult-yank-pop)
         ("C-c C-r" . consult-recent-file))
  :config
  (setq consult-narrow-key "<"
        consult-preview-key "M-."))

;; Embark - actions on completion candidates
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; Embark-Consult integration
(use-package embark-consult
  :ensure t
  :after (embark consult))

;; Minibuffer and scrolling behavior
(setq enable-recursive-minibuffers t
      scroll-conservatively 100
      split-height-threshold 50
      split-width-threshold 200)

;; Writable grep
(use-package wgrep
  :ensure t)

;; Command logging for demonstrations
(use-package command-log-mode
  :ensure t)

;; Persistent scratch buffer
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

;; Centered and focused editing
(use-package centered-cursor-mode
  :ensure t)

(use-package olivetti
  :ensure t)

(provide 'navigation)

;;; navigation.el ends here
