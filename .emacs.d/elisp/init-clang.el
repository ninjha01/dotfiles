(setq c-package-list '(ggtags company-c-headers clang-format))
(dolist (package c-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))
(require 'ggtags)
(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; Autoformat
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
    (lambda ()
      (progn
        (when (locate-dominating-file "." ".clang-format")
          (clang-format-buffer))
        ;; Continue to save.
        nil))
    nil
    ;; Buffer local hook.
    t))

;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

;; company
(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-c-headers)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

(define-key c-mode-map (kbd "C-c C-c") 'recompile)
(provide 'init-clang)
