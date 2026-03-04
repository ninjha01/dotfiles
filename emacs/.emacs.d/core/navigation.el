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

;;; Ivy, Counsel, Swiper for completion
(use-package ivy
  :init (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-git-grep)
         ("C-r" . counsel-minibuffer-history)))

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

(use-package centered-cursor-mode
  :if (display-graphic-p))
(use-package olivetti)

(provide 'navigation)
;;; navigation.el ends here
