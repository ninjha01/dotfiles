(setq ocaml-package-list '(tuareg ocamlformat)) 
(dolist (package ocaml-package-list) 
  (unless (package-installed-p package) 
    (package-install package))) 

(add-hook 'tuareg-mode-hook 
	  (lambda () 
	    "Triangle pipeline symbol"
	    (push '("|>" . "▶") prettify-symbols-alist) 
	    (push '("->" . "→") prettify-symbols-alist) 
	    (prettify-symbols-mode))) 

(defun my-tuareg-mode-before-save-hook () 
  (when (eq major-mode 'tuareg-mode) 
    (ocp-indent-buffer) 
    (ocamlformat)))

(add-hook 'before-save-hook #'my-tuareg-mode-before-save-hook)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(provide 'init-ocaml)
