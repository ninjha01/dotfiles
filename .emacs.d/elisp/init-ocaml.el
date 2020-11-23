(setq ocaml-package-list '(tuareg))
(dolist (package ocaml-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(add-hook 'tuareg-mode-hook (lambda ()
			      "Triangle pipeline symbol"
			      (push '("|>" . "▶") prettify-symbols-alist)
			      (push '("->" . "→") prettify-symbols-alist)
			      (prettify-symbols-mode)))
(provide 'init-ocaml)
