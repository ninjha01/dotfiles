;; conseq major mode, conseq-mode

(setq conseq-font-lock-keywords (let* (
				       ;; categories of keywords
				       (conseq-keywords '("include" "inputs+" "outputs+" "run"
							  "executor" "eval")) 
				       (conseq-types '("let")) 
				       (conseq-functions '("rule" "add-if-missing"
							   "remember-executed"))
				       ;; generate regex for each category
				       (conseq-comments-regexp '("#.+")) 
				       (conseq-keywords-regexp (regexp-opt conseq-keywords 'words)) 
				       (conseq-types-regexp (regexp-opt conseq-types 'words)) 
				       (conseq-functions-regexp (regexp-opt conseq-functions
									    'words))) 
				  `(
				    ;; order matters, once colored, text can't be recolored
				    (, conseq-comments-regexp . font-lock-comment-face) 
				    (, conseq-keywords-regexp . font-lock-keyword-face) 
				    (, conseq-types-regexp . font-lock-type-face) 
				    (, conseq-functions-regexp . font-lock-function))))

(define-derived-mode conseq-mode fundamental-mode 
  "conseq"
  "Major mode for editing conseq files." 
  (setq font-lock-defaults '(conseq-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.conseq\\'" . conseq-mode))

(provide 'conseq-mode)
