(setq keyfreq-package-list '(keyfreq))
(dolist (package keyfreq-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
(setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
;;;; Ignore scrolling
(setq keyfreq-excluded-commands '(mwheel-scroll))

(provide 'init-keyfreq)
