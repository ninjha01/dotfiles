;; Package system
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq package-list '(blacken cargo lsp-mode lsp-ui keyfreq ace-window beacon browse-kill-ring company company-go company-shell
				company-web counsel docker dockerfile-mode dumb-jump elisp-format
				elpy fireplace flycheck-rust flyparens forge god-mode helm-flycheck
				ivy js-comint magit magit-todos magit-topgit markdown-mode mood-line
				multiple-cursors prettier-js projectile rust-mode tide todoist
				use-package vlf web-mode which-key yaml-mode))
(dolist (package package-list) 
  (unless (package-installed-p package) 
    (package-install package)))


;; General

;; subword mode
(global-subword-mode 1)

;;; KeyFreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;;  Projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode 1)


;;; Browse Kill ring
(global-set-key (kbd "C-c y") 'browse-kill-ring)


					;:; God mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)


;;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;;;; enable this if you want `swiper' to use it
;;;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


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


;;; Org mode
(require 'org)
(setq org-agenda-files (quote ("~/Google Drive/org/work.org")))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;;; Fuzzy Matching
;;; TODO: Is this unneccasrry w/ counsel?
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;;; Go to home dir when typing ~ in file completion
(add-hook 'ido-setup-hook
	  (lambda ()
	    ;; Go straight home
	    (define-key ido-file-completion-map
	      (kbd "~")
	      (lambda ()
		(interactive)
		(if (looking-back "/")
		    (insert "~/")
		  (call-interactively 'self-insert-command))))))


;;; Opening Large Files
(require 'vlf-setup)


;;; COMPANY
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)



;;; global hl line mode
(global-hl-line-mode t)


;;; global prettify symbols mode
(global-prettify-symbols-mode t)


;;; beacon mode 
(beacon-mode t)


;;; mood line
(mood-line-mode)


;;; tool bar mode
(tool-bar-mode -1)


;;; scroll bar mode
(scroll-bar-mode -1)


;; Dev


;;; Magit
(magit-todos-mode 1)
(global-set-key (kbd "C-x g") 'magit-status)
(setq ediff-split-window-function 'split-window-horizontally) ;; Better for wide monitor
(setq ediff-merge-split-window-function 'split-window-vertically) ;; Better for wide monitor

(add-hook 'after-save-hook 'magit-after-save-refresh-status t) ;; update magit on save

(with-eval-after-load 'magit
  (require 'forge))


;;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'go-mode 'flycheck-mode)
(add-hook 'python-mode 'flycheck-mode)
(add-hook 'python-mode-hook
          (lambda ()
	    (elpy-mode 1)
	    (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
	    (setq flycheck-pylintrc "~/.pylintrc")))


;;; Python


;;;; elpy
(setq python-shell-interpreter "python3"
      elpy-rpc-python-command "python3"
      python-shell-interpreter-args "-i")

(setenv "WORKON_HOME" "~/miniconda3/envs/")

;; flycheck instead of flymake
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)

;; Black
(add-hook 'elpy-mode-hook
	  '(lambda ()
	     (when (eq major-mode 'python-mode)
	       (add-hook 'before-save-hook 'elpy-black-fix-code))))



;;; Web Dev

;;;; Use web-mode for...
(require 'web-mode)
(setq web-mode-enable-auto-closing t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

;;;; Set up web mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;;; Indentation
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

;;;; Tide Mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)


;;;;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)


;;;;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


;;;; Yaml Mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(put 'erase-buffer 'disabled nil)


;;;; Docker
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


;;; Lisp


;;;; Use clisp for run-lisp
(setq inferior-lisp-program "clisp")


;;; Rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;; Indentation
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;;;; Rustfmt
(setq rust-format-on-save t)

;;;; Cargo minor mode
(add-hook 'rust-mode-hook 'cargo-minor-mode)
