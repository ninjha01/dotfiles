(use-package keyfreq
  :ensure t
  :config
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
  (setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
;;;; Ignore scrolling
  (setq keyfreq-excluded-commands '(mwheel-scroll)))

(keyfreq-mode 1)


(provide 'init-keyfreq)
