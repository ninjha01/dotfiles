;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                Nishant's Emacs Configuration                  ;;
;;                    There are many like it                     ;;
;;                     But this one is mine                      ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package system
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq package-list '(ivy mood-line elpy beacon ace-window which-key company rust-mode yaml-mode web-mode vlf smooth-scroll multiple-cursors markdown-mode helm-flycheck company-web company-shell company-go))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Aesthetics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode t)
(global-prettify-symbols-mode t)
(beacon-mode t)
(mood-line-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;;Clear Scratch
(setq initial-scratch-message "")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("acaccddbc0ae7d5c2cdea2e64b0261ca383671205752c062c44590d944ad0842" default)))
 '(package-selected-packages
   (quote
    (prettier-js tide js-comint flycheck-rust beacon ace-window magit magit-topgit yaml-mode which-key web-mode vlf use-package smooth-scroll rust-mode multiple-cursors markdown-mode helm-flycheck flyparens company-web company-shell company-go)))
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

;; Behavior Modification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
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
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Electric mode
(electric-pair-mode 1)

;; Open and name shell
(global-set-key (kbd "M-s M-s") (lambda () (interactive) (shell) (rename-uniquely)))

;; Save point position between sessions
(save-place-mode 1)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs-saves/" t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Don't jump around on scroll
(setq scroll-conservatively 100)

;; Window Manipulation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; better window switching
(global-set-key (kbd "C-x o") 'ace-window)

;; move with S-r, S-l, etc.
(windmove-default-keybindings 'super)

;; Expand windows
(global-set-key (kbd "C-^") 'enlarge-window)
;;(global-set-key (kbd "C-") 'shrink-window)
(global-set-key (kbd "C-,") 'shrink-window-horizontally)
(global-set-key (kbd "C-.") 'enlarge-window-horizontally)

;; Window manipulation undo tree
(winner-mode 1)

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))
(global-set-key (kbd "C-S-r") 'rotate-windows)
;; Buffer Manipulation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Movement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rearrange lines up and down
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key (kbd "M-l") 'goto-line-with-feedback)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Opening newlines
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Cut and paste from registers 1-3
(defun copy-to-register-1 ()
  "Copy current line or text selection to register 1."
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (line-beginning-position))
             (setq $p2 (line-end-position))))
    (copy-to-register ?1 $p1 $p2)
    (message "Copied to register 1: 「%s」." (buffer-substring-no-properties $p1 $p2))))

(global-set-key (kbd "C-x c 1") 'copy-to-register-1)

(defun paste-from-register-1 ()
  "Paste text from register 1."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(global-set-key (kbd "C-x p 1") 'paste-from-register-1)

(defun copy-to-register-2 ()
  "Copy current line or text selection to register 2."
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (line-beginning-position))
             (setq $p2 (line-end-position))))
    (copy-to-register ?2 $p1 $p2)
    (message "Copied to register 2: 「%s」." (buffer-substring-no-properties $p1 $p2))))

(global-set-key (kbd "C-x c 2") 'copy-to-register-2)
(defun paste-from-register-2 ()
  "Paste text from register 2."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?2 t))

(global-set-key (kbd "C-x p 2") 'paste-from-register-2)

(defun copy-to-register-3 ()
  "Copy current line or text selection to register 3."
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (line-beginning-position))
             (setq $p2 (line-end-position))))
    (copy-to-register ?3 $p1 $bp2)
    (message "Copied to register 3: 「%s」." (buffer-substring-no-properties $p1 $p2))))

(global-set-key (kbd "C-x c 3") 'copy-to-register-3)

(defun paste-from-register-3 ()
  "Paste text from register 3."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?3 t))

(global-set-key (kbd "C-x p 3") 'paste-from-register-3) 

;; Mode Manipulation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Subword-mode CamelCase handling
(require 'subword)
(subword-mode 1)

;;Which-key
(require 'which-key)
(which-key-mode 1)

;;Org mode
(require 'org)
(setq org-agenda-files (quote ("~/ledger/todo.org")))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

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

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(add-hook 'python-mode-hook
          (lambda ()
	    (elpy-mode 1)
	    (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
	    (setq flycheck-pylintrc "~/.pylintrc")))

;; Fuzzy Matching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Opening Large Files
(require 'vlf-setup)

;; COMPANY
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; FLYCHECK
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'python-mode 'flycheck-mode)
(add-hook 'go-mode 'flycheck-mode)

;; Use web-mode for...
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

;; Use yaml-mode for...
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(put 'erase-buffer 'disabled nil)

;; Use clisp for run-lisp
(setq inferior-lisp-program "clisp")

;; ELPY
(setq python-shell-interpreter "python3"
      elpy-rpc-python-command "python3"
      python-shell-interpreter-args "-i")
(setenv "WORKON_HOME" "~/miniconda3/envs/")

;; Tide Mode
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

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


;; macOS specific ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; keybindings
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; macOS shell tomfoolery
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setq shell-command-switch "-ic")
(put 'downcase-region 'disabled nil)
