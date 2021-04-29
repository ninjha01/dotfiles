(setq company-package-list '(company company-go company-shell company-web company-c-headers company-emoji))
(dolist (package company-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company (define-key company-active-map (kbd "C-n") #'company-select-next) 
		      (define-key company-active-map (kbd "C-p") #'company-select-previous))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Speed up prompt
(setq company-idle-delay 0)


;; start after 2 char
(setq company-minimum-prefix-length 2)

;; emoji
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))


;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)


(provide 'init-company)
