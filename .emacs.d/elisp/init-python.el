(setq py-package-list '(blacken elpy jedi py-isort))
(dolist (package py-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(add-hook 'python-mode-hook 
	  (lambda () 
	    (elpy-enable)))


;;;; elpy
(setq elpy-rpc-virtualenv-path 'current)
(setq python-shell-interpreter "python3" elpy-rpc-python-command "python3"
      python-shell-interpreter-args "-i")
(setenv "WORKON_HOME" "/usr/local/Caskroom/miniconda/base/envs/")
(setq flycheck-flake8rc "~/dotfiles/python/.flake8")
(require 'elpy)
(add-hook 'elpy-mode-hook 
	  '(lambda () 
	     (add-hook 'before-save-hook 'elpy-black-fix-code 'make-it-local)))
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(defalias 'workon 'pyvenv-workon)

(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
        (error (elpy-rgrep-symbol
                (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)

;; Suppress error in process sentinel: elpy-rpc--default-error-callback: peculiar error: "exited abnormally with code 1"
;; https://github.com/jorgenschaefer/elpy/issues/1749
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))


;; Suppress readline incompatibility error
;; , yet ‘python-shell-completion-native-enable’ was t and "python3" is not part of the ‘python-shell-completion-native-disabled-interpreters’ list.  Native completions have been disabled locally. 
(setq python-shell-completion-native-disabled-interpreters '("python3"))

(provide 'init-python)
