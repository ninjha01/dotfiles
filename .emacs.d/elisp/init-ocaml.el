(setq ocaml-package-list '(ggtags company-c-headers clang-format))
(dolist (package ocaml-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(provide 'init-ocaml)
