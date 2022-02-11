(use-package 
  company 
  :ensure t 
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; Speed up prompt
  (setq company-idle-delay 0.1)


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
  (add-hook 'after-make-frame-functions '--set-emoji-font))
(global-company-mode)


(provide 'init-company)
