(setq ocaml-package-list '(tuareg ob-ocaml))
(dolist (package ocaml-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(add-hook 'tuareg-mode-hook (lambda ()
			      "Triangle pipeline symbol"
			      (push '("|>" . "▶") prettify-symbols-alist)
			      (prettify-symbols-mode)))
(provide 'init-ocaml)
