(add-to-list 'load-path "~/.emacs.d/elisp")
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

(when (string-equal (getenv "SKIP_REPOS_DOWNLOAD") "true")
  (setq straight-check-for-modifications nil))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t)
  :config
  (setq straight-vc-git-default-protocol 'ssh))

(defun my-straight-process-run-error-handler (func &rest args)
  "Catch error from `straight--process-run', print process buffer, and re-signal error."
  (condition-case err
      (apply func args)
    (error (progn
             (switch-to-buffer straight-process-buffer)
             (error "Caught error: %s" err)))))

(advice-add 'straight--process-run :around #'my-straight-process-run-error-handler)


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
(use-package
  shrink-path :ensure t
  :straight (:host github :repo "zbelial/shrink-path.el" :files ("dist" "*.el")))

(use-package
  all-the-icons :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (setq dnoom-modeline-height 22)
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
(menu-bar-mode -1)
;;; Remove Toolbar
(tool-bar-mode -1)
;;; Remove scroll bar
(scroll-bar-mode -1)

;; remove fringe color
;; https://emacs.stackexchange.com/questions/5342/how-do-i-set-the-fringe-colors-to-whatever-is-the-background-color
(set-face-attribute 'fringe nil
                    :background nil)

;; Don't show gaps on resize
(setq frame-resize-pixelwise t)

;; Delimiters
(show-paren-mode)
(use-package
  rainbow-delimiters :ensure t)

(rainbow-delimiters-mode)
(show-paren-mode)

(setq inhibit-startup-screen t)

;; UX
(setq electric-indent-mode nil)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Clear Scratch
(setq initial-scratch-message "")

;; Movement
(use-package ace-window
  :ensure t
  :bind (:map global-map
              ("C-x o" . ace-window)))

;;
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-2") 'split-window-below)
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

;; Enable hs-minor-mode for programming modes as an example
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; Globally bind the key for toggling code folding
(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))
(global-set-key (kbd "<backtab>") 'toggle-fold)
(global-set-key (kbd "C-c <tab>") 'hs-show-all)
(global-set-key (kbd "C-c <backtab>") 'hs-hide-all)

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

(defun hash-file-name (file-path)
  (let ((hashed-name (sha1 file-path)))
    ;; Ensure the mapping directory exists
    (unless (file-exists-p "~/.emacs.d/emacs-saves/")
      (make-directory "~/.emacs.d/emacs-saves/" t))
    ;; Ensure the hash file exists
    (unless (file-exists-p "~/.emacs.d/emacs-saves/file_to_hash_map.txt")
      (with-temp-buffer (write-file "~/.emacs.d/emacs-saves/file_to_hash_map.txt" t)))

    (with-temp-file "~/.emacs.d/emacs-saves/file_to_hash_map.txt"
      (insert-file-contents "~/.emacs.d/emacs-saves/file_to_hash_map.txt")
      (goto-char (point-max))
      (insert (format "%s -> %s\n" file-path hashed-name)))
    hashed-name))

;; sometimes the filename is too long, so we hash the filename and store a record to lookup the filename later
(setq auto-save-file-name-transforms
      `((".*" ,(concat "~/.emacs.d/emacs-saves/" (hash-file-name "\\1")) t)))
(setq auto-save-default t)
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Don't jump around on scroll
(setq scroll-conservatively 100)

;; Add logging for when emacs hangs
(setq-default garbage-collection-messages t)

;; Copy current filename
(defun copy-file-path
    (&optional
     @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
   Result is full path.
   If `universal-argument' is called first, copy only the dir path.
   If in dired, copy the file/dir cursor is on, or marked files.
   If a buffer is not file and not dired, copy value of `default-directory' (which is usually  dir when that buffer was created)
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
    (kill-new (if @dir-path-only-p (progn (message "Directory path copied: Currentthe%s "
                                                   (file-name-directory $fpath))
                                          (file-name-directory $fpath))
                (progn (message "File path copied: %s" $fpath) $fpath )))))

(setq trash-directory "~/.Trash")
(setq split-height-threshold 50           ; lines to place window below
      split-width-threshold 200)   ; cols to place window to the right

;; Highlighted regions are grey with white text
(set-face-attribute 'region nil
                    :background "#666"
                    :foreground "#ffffff")

(use-package centered-cursor-mode
  :ensure t)

(use-package olivetti
  :ensure t)

;; Programming

(use-package magit-todos
  :ensure t
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:"))

(use-package magit
  :ensure t
  :bind (:map global-map
	      ("C-x g" . magit-status)
	      ("C-c g" . magit-file-dispatch)
	      ("C-c b" . magit-blame))
  :config
  (require 'magit-todos)
  (setq magit-save-repository-buffers 'dontask)
  :hook (after-save-hook .
			 (lambda ()
			   (setq magit-after-save-refresh-status t))))
(use-package markdown-mode
  :ensure t
  :mode (".md")
  :hook ((markdown-mode . (lambda ()
                            (flycheck-mode 1)
                            (company-mode -1) ; Disable company-mode
                            (copilot-mode -1) ; Disable copilot-mode
                            (olivetti-mode 1) ; Enable olivetti-mode
                            ))))

(use-package flycheck-aspell
  :ensure t)

;; Orgmode
(use-package org
  :mode (".org")
  :ensure org-contrib
  :config (setq org-directory "~/Google Drive/My Drive/org")
  (setq org-bullets-mode 1)
  (setq auto-revert-mode 1)
  (setq org-indent-mode 1)
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

(use-package git-link
  :ensure t
  :bind (:map global-map
	      ("C-c l" . git-link)))


(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  :hook
  ((prog-mode . copilot-mode)))

(use-package gptel
  :straight (:host github :repo "karthink/gptel" :files ("*.el"))
  :ensure t
  :bind
  (:map global-map
	("C-c C-g" . gptel))
  :init
  ;; (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-default-mode 'markdown-mode)
  (setq-default
   gptel-model "claude-3-5-sonnet-20240620"
   gptel-backend (gptel-make-anthropic 
		     "Claude"
		   :stream t 
		   :key (getenv "CLAUDE_API_KEY")))
  (gptel-make-ollama "Ollama"             
    :host "localhost:11434"               
    :stream t                             
    :models 
    '("codellama:34b"))
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (require 'markdown-mode)
  (define-key markdown-mode-map (kbd "C-<return>") 'gptel-send)
  (define-key markdown-mode-map (kbd "C-c C-c") 'gptel-send))

;; LSP
(defun my/lsp-mode-should-not-activate ()
  "Return non-nil if the current buffer should not activate lsp-mode."
  (and (boundp 'tramp-file-name-regexp)
       buffer-file-name
       (string-match-p tramp-file-name-regexp buffer-file-name)))
(defun my/lsp-deferred ()
  "Call `lsp-deferred` if the current buffer is not a TRAMP buffer."
  (unless (my/lsp-mode-should-not-activate)
    (lsp-deferred)))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :bind 

  (:map lsp-mode-map)
  ("C-<return>" . lsp-execute-code-action)
  :config
  (remove-hook 'before-save-hook 'lsp-format-buffer)
  (lsp-enable-which-key-integration t)
  (setq lsp-auto-guess-root t)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-file-watchers nil)
  ;; Reduce the amount of clutter and verbosity
  (setq lsp-modeline-code-actions-enable nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil)
  :hook ((java-mode . my/lsp-deferred)
         (web-mode . my/lsp-deferred)
         (swift-mode . my/lsp-deferred)
         (typescript-mode . my/lsp-deferred)
         (tide-mode . my/lsp-deferred)
         (python-mode . my/lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)))


(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-next))
  ;; (:map lsp-mode-map
  ;;  ("C-c <tab>" . company-indent-or-complete-common))
  :config (setq company-tooltip-align-annotations t)
  :custom (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
	      ("C-c e" . flycheck-next-error)
	      ("C-c C-e" . 'flycheck-list-errors)))

(use-package apheleia
  :ensure t
  :config
  (add-to-list 'apheleia-mode-alist
               '(swift-mode . swift-format))

  (add-to-list 'apheleia-formatters
               '(swift-format "swift-format" (buffer-file-name)))
  
  (defvar prettier-config-frontend t
    "Toggle between frontend and root package.json for prettier config.")

  (defun toggle-prettier-config-path ()
    "Toggle prettier config path between frontend and root package.json."
    (interactive)
    (setq prettier-config-frontend (not prettier-config-frontend))
    (setf (alist-get 'prettier apheleia-formatters)
          `(npx "prettier"
                "--config" ,(if prettier-config-frontend
                                (concat (projectile-project-root) "frontend/package.json")
                              (concat (projectile-project-root) "package.json"))
                "--stdin-filepath" filepath))
    (message "Prettier config path set to %s"
             (if prettier-config-frontend "frontend/package.json" "package.json")))

  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--config" (concat (projectile-project-root) "frontend/package.json") "--stdin-filepath" filepath))
  
  (add-to-list 'apheleia-mode-alist
               '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist
               '(markdown-mode . prettier))
  (apheleia-global-mode t))

;; Swift
(use-package swift-mode
  :ensure t
  :mode
  ("\\.swift\\'" . swift-mode)
  :hook 
  (swift-mode . (lambda () 
		  (lsp)))
  :config
  (defun xcode-open-current-file()
    (interactive)
    (shell-command-to-string
     (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name))))

  (defun xcode-build() 
    (interactive) 
    (shell-command-to-string "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))


  (defun xcode-run() 
    (interactive) 
    (shell-command-to-string "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
  ;; Key bindings
  :bind 
  (:map swift-mode-map 
	("C-c C-c" . xcode-run) 
	("C-c C-b" . xcode-build) 
	("C-c C-o" . xcode-open-current-file)))

(use-package flycheck-swift
  :ensure t
  :after flycheck
  :hook 
  (flycheck-mode . flycheck-swift-setup)
  (swift-mode . flycheck-mode)
  :config
  (setq flycheck-swift-sdk-path "/Users/nishantjha/Desktop/Xcode-beta.app/Contents/Developer/Platforms/XROS.platform/Developer/SDKs/XROS1.0.sdk")
  ;;   Select the appropriate SDK version you use
  (setq flycheck-swift-target "arm64-apple-xros"))

(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

;; Go
(use-package go-mode
  :ensure t
  :hook ((before-save . gofmt-before-save)  ;; Format before saving
         (go-mode . lsp-deferred))           ;; Enable LSP for go-mode
  :config)


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  ;; Disable automatic popups
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil)

  ;; Add a keybinding for showing documentation
  (define-key lsp-ui-mode-map (kbd "C-c C-d") 'lsp-ui-doc-show))





(use-package company
  :ensure t
  :config
  ;; Use company for completion
  (global-company-mode)
  )

(use-package yasnippet
  :ensure t
  :config
  ;; Use yasnippet for code templates
  (yas-global-mode)
  )

;; Python

(defun my-lsp-describe-at-point ()
  "Show LSP documentation in the minibuffer, with Markdown and HTML formatting stripped."
  (interactive)
  (let ((response (lsp-request "textDocument/hover" (lsp--text-document-position-params))))
    (if response
        (let ((contents-hash (gethash "contents" response)))
          (if (and contents-hash (hash-table-p contents-hash))
	      (let ((doc-string (gethash "value" contents-hash)))
                ;; Remove Markdown formatting and replace HTML entities
		(setq doc-string (replace-regexp-in-string "```python" "" doc-string)) ; remove code block beginnning
		(setq doc-string (replace-regexp-in-string "```" "" doc-string)) ; remove code block ending
                (setq doc-string (replace-regexp-in-string "---" "" doc-string)) ; remove horizontal rules
                (setq doc-string (replace-regexp-in-string "&nbsp;" " " doc-string)) ; replace &nbsp; with space
                ;; Display processed string
                (message "%s" (or doc-string "Documentation not available.")))
            (message "Documentation not available.")))
      (message "No response from LSP server."))))


(use-package pyenv
  :ensure t)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :bind (:map lsp-mode-map
	      ("C-c d" . my-lsp-describe-at-point))
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3")))

(require 'flycheck-ruff)


(use-package py-isort
  :ensure t
  :after python
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))


(use-package poetry
  :ensure t
  :hook (python-mode . poetry-tracking-mode))

(use-package dockerfile-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)


;; ;; Web Dev
(use-package web-mode
  :ensure t
  :mode 
  ("\\.html\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  ("\\.jsx\\'" . web-mode)
  ("\\.js\\'" . web-mode)
  ("\\.ts\\'" . web-mode)
  ("\\.cjs\\'" . web-mode)
  ("\\.mjs\\'" . web-mode)
  ("\\.json\\'" . web-mode)
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
                    (lsp-deferred)
                    (when (string-equal "tsx" (file-name-extension buffer-file-name))
		      (setup-tide-mode)))))

(use-package typescript-mode
  :ensure t
  :config (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package tide
  :ensure t
  :hook
  (typescript-mode . tide-setup)
  :bind
  (:map tide-mode-map)
  ;; bind C-<return> to tide fix
  ("C-<return>" . tide-fix)
  :init (defun setup-tide-mode ()
          (interactive)
          (tide-setup)
          (flycheck-mode +1)
          (setq flycheck-check-syntax-automatically '(save mode-enabled))
	  (flycheck-select-checker 'typescript-tide)
          (eldoc-mode +1)
          (tide-hl-identifier-mode +1)
          (company-mode +1)))



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
    (shell-command "open -a Warp \"$pwd\""))
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


(use-package shell
  :ensure nil
  :hook
  (shell-mode . (lambda ()
                  ;; Disable font-lock mode
                  (font-lock-mode -1)
                  ;; Add hook to truncate buffer
                  (add-hook 'comint-output-filter-functions
                            'comint-truncate-buffer t t)))
  :config
  ;; Set maximum buffer size
  (setq comint-buffer-maximum-size 5000))


;; (use-package yafolding
;;   :ensure t
;;   :hook (prog-mode . yafolding-mode))

;; TODO Graphviz
(use-package graphviz-dot-mode
  :ensure t)

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

(use-package csv-mode
  :ensure t)

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :config (setq projectile-indexing-method 'native)
  (add-to-list 'projectile-globally-ignored-directories "Pods")
  (add-to-list 'projectile-globally-ignored-directories ".next")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "straight")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".mypy_cache")
  (setq projectile-git-submodule-command nil)
  (setq projectile-require-project-root t)
  (setq projectile-ignored-projects '("~/" "/opt/homebrew"))
  (require 'magit)
  (setq projectile-switch-project-action 'magit-status))


;; convenience functions
(defun connect-to-tatta-gpu ()
  "Connect to Nishant's server via TRAMP."
  (interactive)
  (let ((server-ip (getenv "TATTA_GPU_IP"))
        (tramp-default-method "ssh"))
    (if server-ip
	(find-file (format "/ssh:nishant@%s#22:/home/nishant/tatta-preview" server-ip))
      (message "TATTA_GPU_IP environment variable is not set."))))

(projectile-mode 1)
(message "reached end of init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
