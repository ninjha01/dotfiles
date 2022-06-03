
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; Suprres native comp warnings buffer
(setq warning-minimum-level 
      :error)

(package-initialize)

;; UI

;; Theme
(use-package zenburn-theme 
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package display-line-numbers
  :commands (display-line-numbers-mode)
  :config
  (set-face-foreground 'line-number "#7F9F7F")
  :init
  (defun goto-line-with-feedback () 
    "Show line numbers temporarily, while prompting for the line number input" 
    (interactive) 
    (unwind-protect
	(progn (display-line-numbers-mode 1)
	       (goto-line (read-number "Goto line: "))) 
      (display-line-numbers-mode -1))) 
  (global-set-key (kbd "M-l") 'goto-line-with-feedback))

;; Modeline

(use-package doom-modeline 
  :ensure t 
  :init (doom-modeline-mode t) 
  :config (setq doom-modeline-height 22) 
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
  (set-face-attribute 'mode-line nil 
		      :font "Fira Code 12"))

;; Font
(use-package 
    fira-code-mode :ensure t
    :init
    (fira-code-mode)
    :config
    ;; (fira-code-mode-install-fonts t) ;; Instal if you haven't already
    (setq default-frame-alist '((font . "Fira Code 12"))))

;; Chrome
;;; Remove menubar
(menu-bar-mode -1) ;;; Remove Toolbar
(tool-bar-mode -1) ;;; Remove scroll bar
(scroll-bar-mode -1)

;; remove fringe color
;; https://emacs.stackexchange.com/questions/5342/how-do-i-set-the-fringe-colors-to-whatever-is-the-background-color
(set-face-attribute 'fringe nil 
		    :background nil)

;; Don't show gaps on resize
(setq frame-resize-pixelwise t)

;;; Delimiters
(show-paren-mode)
(use-package 
    rainbow-delimiters :ensure t)

(rainbow-delimiters-mode)
(show-paren-mode)

(setq inhibit-startup-screen t)

;; UX
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;;Clear Scratch
(setq initial-scratch-message "")

;; Movement
(use-package ace-window 
  :ensure t
  :bind (:map global-map
	      ("C-x o" . ace-window)))

(save-place-mode 1)

;; show commands
(use-package command-log-mode 
  :ensure t)

(use-package persistent-scratch 
  :ensure t)

(persistent-scratch-setup-default)
(persistent-scratch-autosave-mode 1)

(use-package multiple-cursors 
  :ensure t 
  :bind (:map global-map
	      ("C->" . mc/mark-next-like-this) 
	      ("C-<" . mc/mark-previous-like-this)))

(use-package subword)

(global-subword-mode)

(use-package which-key 
  :ensure t)

(which-key-mode 1)

(global-hl-line-mode t)
(global-prettify-symbols-mode t)

(use-package beacon 
  :ensure t)

(beacon-mode t)

;; Rebinds
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; Rotate Windows
(defun rotate-windows () 
  "Rotate your windows" 
  (interactive) 
  (cond ((not (> (count-windows) 1)) 
	 (message "You can't rotate a single window!")) 
	(t 
	 (setq i 1) 
	 (setq numWindows (count-windows)) 
	 (while  (< i numWindows) 
	   (let* ((w1 (elt (window-list) i)) 
		  (w2 (elt (window-list) 
			   (+ (% i numWindows) 1))) 
		  (b1 (window-buffer w1)) 
		  (b2 (window-buffer w2)) 
		  (s1 (window-start w1)) 
		  (s2 (window-start w2))) 
	     (set-window-buffer w1  b2) 
	     (set-window-buffer w2 b1) 
	     (set-window-start w1 s2) 
	     (set-window-start w2 s1) 
	     (setq i (1+ i)))))))

(global-set-key (kbd "C-S-r") 'rotate-windows)

;; Query regexp replace
(global-set-key (kbd "C-c r") 'query-replace-regexp)

;; move text up and down

(use-package move-text
  :ensure t
  :bind
  (:map global-map
	("M-S-<up>" . move-text-up)
	("M-S-<down>" . move-text-down)))

;; Cycle space
(global-set-key (kbd "M-SPC") 'cycle-spacing)

(use-package avy :bind (:map global-map
			     ("M-s" . 'avy-goto-char-timer)))

(use-package crux 
  :ensure t 
  :bind (:map global-map
	      ("C-x C-r" . crux-rename-file-and-buffer) 
	      ("C-x C-k" . crux-delete-buffer-and-file)))

(use-package wgrep 
  :ensure t)

(use-package ivy 
  :ensure t 
  :config (setq ivy-use-virtual-buffers t) 
  (setq enable-recursive-minibuffers t) 
  :bind (:map global-map
	      ("C-s" . swiper) 
	      ("C-c C-r" . ivy-resume) 
	      ("C-x b" . ivy-switch-buffer)))

(ivy-mode)

(use-package counsel 
  :ensure t 
  :config 
  :bind (:map global-map
	      ("M-x" . counsel-M-x) 
	      ("C-x C-f" . counsel-find-file) 
	      ("C-c k" . counsel-git-grep) 
	      ("C-r" . counsel-minibuffer-history)))

;; Write backup files to own directory
(unless (file-exists-p "~/.emacs.d/.saves/") 
  (make-directory "~/.emacs.d/.saves/"))

(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

(unless (file-exists-p "~/.emacs.d/emacs-saves/") 
  (make-directory "~/.emacs.d/emacs-saves/"))

(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/emacs-saves/" t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Don't jump around on scroll
(setq scroll-conservatively 100)

;; Add logging for when emacs hangs
(setq-default garbage-collection-messages t)

;; 
;; Copy current filename
(defun copy-file-path 
    (&optional 
     @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
   Result is full path.
   If `universal-argument' is called first, copy only the dir path.
   If in dired, copy the file/dir cursor is on, or marked files.
   If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)
   URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
   Version 2017-09-01" 
  (interactive "P") 
  (let (($fpath (if (string-equal major-mode 'dired-mode) 
		    (progn (let (($result (mapconcat 'identity (dired-get-marked-files) "\n"))) 
			     (if (equal (length $result) 0) 
				 (progn default-directory ) 
			       (progn $result)))) 
		  (if (buffer-file-name) 
		      (buffer-file-name) 
		    (expand-file-name default-directory))))) 
    (kill-new (if @dir-path-only-p (progn (message "Directory path copied: 「%s」"
						   (file-name-directory $fpath)) 
					  (file-name-directory $fpath)) 
		(progn (message "File path copied: 「%s」" $fpath) $fpath )))))

(setq trash-directory "~/.Trash")
(setq split-height-threshold 50	   ; lines to place window below
      split-width-threshold 200)   ; cols to place window to the right


;; Highlighted regions are grey with white text
(set-face-attribute 'region nil 
		    :background "#666" 
		    :foreground "#ffffff")

;; Programming
(use-package magit 
  :ensure t 
  :bind (:map global-map
	      ("C-x g" . magit-status) 
	      ("C-c g" . magit-file-dispatch) 
	      ("C-c b" . magit-blame)) 
  :config (setq magit-save-repository-buffers 'dontask) 
  :hook (after-save-hook . 
			 (lambda () 
			   (setq magit-after-save-refresh-status t))))

;; LSP
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l") 
  :config (add-hook 'before-save-hook 'lsp-organize-imports) 
  (lsp-enable-which-key-integration t) 
  (setq lsp-auto-guess-root t) 
  (setq lsp-restart 'auto-restart) 
  (setq lsp-enable-symbol-highlighting nil) 
  (setq lsp-enable-on-type-formatting nil) 
  (setq lsp-idle-delay 0.5) 
  (setq lsp-headerline-breadcrumb-enable nil) 
  :bind (:map lsp-mode-map
	      ("C-<return>" . lsp-execute-code-action)) 
  :hook ((java-mode . lsp-deferred) 
	 (web-mode . lsp-deferred) 
	 (typescript-mode . lsp-deferred) 
	 (tide-mode . lsp-deferred) 
	 (lsp-mode . lsp-enable-which-key-integration)))

(use-package company 
  :ensure t 
  :hook (prog-mode . company-mode) 
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection) 
	      ("C-n" . company-select-next) 
	      ("C-p" . company-select-next)) 
  ;; (:map lsp-mode-map 
  ;; 	("<tab>" . company-indent-or-complete-common)) 
  :config (setq company-tooltip-align-annotations t) 
  :custom (company-minimum-prefix-length 1) 
  (company-idle-delay 0.1))

(use-package flycheck 
  :ensure t 
  :init (global-flycheck-mode) 
  :bind (:map flycheck-mode-map
	      ("C-c e" . flycheck-next-error) 
	      ("C-c C-e" . 'flycheck-list-errors)))

;; Python
(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))  

(use-package blacken 
  :ensure t 
  :hook ((python-mode . blacken-mode)))

(use-package pyvenv 
  :ensure t 
  :config (setq pyvenv-workon "emacs")  ; Default venv
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniforge/base/envs/") 
  (defalias 'workon 'pyvenv-workon) 
  (pyvenv-tracking-mode 1)) ; Automatically use pyvenv-workon via dir-locals

(use-package python-mode
  :bind
  (:map python-mode-map
	("M-S-<right>" . python-indent-shift-right) 
	("M-S-<left>" . python-indent-shift-left)))

;; ;; Web Dev
(use-package prettier-js 
  :ensure t
  :hook (web-mode . prettier-js-mode))

(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-insert-pair nil)
  (setq sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "Gray")))) ;; Could also have :background "Grey" for example.
  :bind (:map smartparens-mode-map
	      ("C-S-k" . sp-kill-hybrid-sexp)))
  
(use-package web-mode 
  :ensure t 
  :mode (".html$"
	 ".tsx$"
	 ".jsx$"
	 ".json$")
  :config (setq web-mode-markup-indent-offset 2 
		web-mode-code-indent-offset 2
		web-mode-css-indent-offset 2 
		web-mode-enable-css-colorization t
		web-mode-enable-auto-pairing t
		web-mode-enable-auto-indentation nil
		web-mode-enable-comment-keywords t
		web-mode-enable-auto-quoting nil
		web-mode-enable-current-element-highlight t) 
  :hook (web-mode . 
		  (lambda () 
		    (lsp) 
		    (when (string-equal "tsx" (file-name-extension buffer-file-name)) 
		      (setup-tide-mode)))))

(use-package typescript-mode 
  :ensure t 
  :config (setq typescript-indent-level 2) 
  (add-hook 'typescript-mode #'subword-mode))

(use-package tide 
  :ensure t 
  :init (defun setup-tide-mode () 
	  (interactive) 
	  (tide-setup) 
	  (flycheck-mode +1) 
	  (setq flycheck-check-syntax-automatically '(save mode-enabled)) 
	  (eldoc-mode +1) 
	  (tide-hl-identifier-mode +1) 
	  (company-mode +1)) 
  :bind (:map tide-mode-map
	      ("C-<return>" . tide-fix)) 
  :hook ((typescript-mode . tide-setup) 
	 (typescript-mode . tide-hl-identifier-mode)))

;; Orgmode
(use-package org
  :mode (("\\.org$" . org-mode)) 
  :ensure org-contrib 
  :init (defun open-work-org-file () 
	  "Opens ~/Google Drive/org/work.org" 
	  (interactive) 
	  (find-file-other-window "~/google_drive/org/work.org")) 
  :config (setq org-directory "~/google_drive/org") 
  (setq org-bullets-mode 1) 
  (setq auto-revert-mode 1)
  ;; Code blocks indent
  (setq org-src-tab-acts-natively t)
  ;; Code syntax highlight
  (setq org-src-fontify-natively t) 
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t) 
							   (python . t) 
							   (js . t) 
							   (ocaml . t) 
							   (sql . t) 
							   (dot . t) 
							   (plantuml . t))) 
  (setq org-log-done t) 
  (setq org-confirm-babel-evaluate nil) 
  :bind (:map global-map
	      ("C-x C-o" . open-work-org-file))
  (:map org-mode-map
	("C-S-<up>" . org-move-subtree-up)
	("C-S-<down>" . org-move-subtree-down)))

;; Shell

;; (use-package shell
;;   :init
;;   (defun open-shell-buffer-other-window ()
;;     (interactive)
;;     (let ((buf (shell)))
;;     (switch-to-buffer (other-buffer buf))
;;     (switch-to-buffer-other-window buf)))
;;   :bind
;;   (:map global-map
;; 	("C-c t" . open-shell-buffer-other-window)))

(use-package term
  :init
  ;; http://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term/"
  (defun jnm/term-toggle-mode () 
    "Toggles term between line mode and char mode" 
    (interactive) 
    (if (term-in-line-mode) 
	(term-char-mode) 
      (term-line-mode))) 
  (defun open-terminal-dot-app-here () 
    (interactive) 
    (shell-command "open -a Terminal \"$pwd\"")) 
  (defun open-term-here () 
    (interactive) 
    (let ((buf (term "/bin/zsh"))) 
      (switch-to-buffer (other-buffer buf)) 
      (switch-to-buffer-other-window buf))) 
  :bind (:map term-mode-map
	      ("C-c C-j" . jnm/term-toggle-mode)) 
  (:map global-map
	("s-t" . open-terminal-dot-app-here) 
	("C-c t" . open-term-here)))

;; TODO Graphviz

(use-package emacs-lisp-mode
  :straight nil
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-c" . eval-buffer)) 
  :hook (before-save-hook . elisp-format-buffer))

(use-package elisp-format 
  :ensure t)

(use-package yaml-mode 
  :ensure t)

(use-package lua-mode
  :ensure t
  :config
  (defun build-pongdate ()
    (interactive)
    (shell-command "/Users/nishantjha/Desktop/pongdate/build.sh")
    (x-focus-frame nil))
  :hook (after-save-hook . 'build-pongdate))  

(use-package quickrun
  :ensure t
  :bind
  (:map lua-mode-map ("C-c C-c" . quickrun))
  (:map shell-mode-map ("C-c C-c" . quickrun))
  )

(use-package projectile 
  :ensure t 
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)) 
  :config (setq projectile-indexing-method 'native) 
  (add-to-list 'projectile-globally-ignored-directories "Pods") 
  (add-to-list 'projectile-globally-ignored-directories "node_modules") 
  (add-to-list 'projectile-globally-ignored-directories ".mypy_cache") 
  (setq projectile-git-submodule-command nil) 
  (require 'magit) 
  (setq projectile-switch-project-action 'magit-status))

(projectile-mode 1)
(message "reached end of init.el")
