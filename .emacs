;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                Nishant's Emacs Configuration                  ;;
;;	              There are many like it                     ;;
;;	               But this one is mine                      ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Aesthetics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-frame-font "-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1" nil t)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

(global-linum-mode t)
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
 '(inhibit-startup-screen t)
 '(mac-command-modifier nil)
 '(mac-option-modifier (quote meta))
 '(package-selected-packages
   (quote
    (multiple-cursors swift3-mode yaml-mode web-mode vlf smooth-scroll rtags markdown-mode helm flyparens flymd flycheck evil elpy company-c-headers ac-haskell-process)))
 '(proof-splash-enable nil)
 '(tool-bar-mode nil)
 '(vlf-application (quote dont-ask)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Behavior Modification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(global-set-key (kbd "C-h") 'query-replace)

;; Open and name shell command
(global-set-key (kbd "M-s M-s") (lambda () (interactive) (shell) (rename-uniquely)))

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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
    (copy-to-register ?3 $p1 $p2)
    (message "Copied to register 3: 「%s」." (buffer-substring-no-properties $p1 $p2))))

(global-set-key (kbd "C-x c 3") 'copy-to-register-3)

(defun paste-from-register-3 ()
  "Paste text from register 3."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?3 t))

(global-set-key (kbd "C-x p 3") 'paste-from-register-3) 

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

;; Open Files in Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;; Opening Large Files
(require 'vlf-setup)

;; COMPANY
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'python-mode' 'flycheck-mode)

;; Use web-mode for...
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; Use yaml-mode for...
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(put 'erase-buffer 'disabled nil)

;; Use clisp for run-lisp
(setq inferior-lisp-program "clisp")




