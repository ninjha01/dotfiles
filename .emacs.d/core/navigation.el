;;; navigation.el --- Navigation and editor enhancement -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for navigation, window management, and completion.

;;; Code:

;;; Window management
(use-package ace-window
  :bind (("C-x o" . ace-window)
         ("C-3" . split-window-right)
         ("C-2" . split-window-below)
         ("C-S-r" . nj/rotate-windows)))

;; Persist cursor position across sessions
(save-place-mode 1)

;;; Multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;;; Navigate CamelCase words
(use-package subword
  :config (global-subword-mode 1))

;;; Code folding
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (("<backtab>" . nj/toggle-fold)
         ("C-c <tab>" . hs-show-all)
         ("C-c <backtab>" . hs-hide-all)))

;;; Keybinding discovery
(use-package which-key
  :config (which-key-mode 1))

;;; Symbol prettification
(global-prettify-symbols-mode 1)

;;; macOS key modifiers
(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      ns-function-modifier 'hyper)

;;; Move lines/regions
(use-package move-text
  :bind (("M-S-<up>" . move-text-up)
         ("M-S-<down>" . move-text-down)))

;;; Jump to visible text
(use-package avy
  :bind (("M-s" . avy-goto-char-timer)
         ("C-c r" . query-replace-regexp)
         ("M-SPC" . cycle-spacing)))

;;; File utilities
(use-package crux
  :bind (("C-x C-r" . crux-rename-file-and-buffer)
         ("C-x C-k" . crux-delete-buffer-and-file)))

;;; Modern completion stack: Vertico + Consult + Orderless + Embark

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

(use-package savehist
  :ensure nil
  :init (savehist-mode)
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring project-prefix-history-list))
  (history-length 1000))

(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c k" . consult-ripgrep)
         ("C-c f" . consult-find)
         ("M-l" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-y" . consult-yank-pop)
         ("C-c C-r" . consult-recent-file))
  :custom
  (consult-narrow-key "<")
  (consult-preview-key "M-."))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

;;; Minibuffer and scrolling
(setq enable-recursive-minibuffers t
      scroll-conservatively 100
      split-height-threshold 50
      split-width-threshold 200)

;;; Additional editing utilities
(use-package wgrep)
(use-package command-log-mode)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(use-package centered-cursor-mode)
(use-package olivetti)

(provide 'navigation)
;;; navigation.el ends here
