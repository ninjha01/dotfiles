;; python
(require 'lsp)
(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
		   (require 'flycheck)
                   (lsp))))

(use-package
  blacken
  :demand t
  :hook ((python-mode . blacken-mode)))
(use-package 
  pyvenv 
  :demand t 
  :config (setq pyvenv-workon "emacs")  ; Default venv
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniforge/base/envs/") 
  (defalias 'workon 'pyvenv-workon) 
  (pyvenv-tracking-mode 1)) ; Automatically use pyvenv-workon via dir-locals


(define-key python-mode-map (kbd "M-S-<right>") 'python-indent-shift-right)
(define-key python-mode-map (kbd "M-S-<left>") 'python-indent-shift-left)


(provide 'init-python)
