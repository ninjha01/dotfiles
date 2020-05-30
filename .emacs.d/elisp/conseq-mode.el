;; conseq major mode, conseq-mode

(setq conseq-highlights
      '(
	("#.+" . font-lock-comment-face)
	("rule\\|add-if-missing\\|remember-executed" . font-lock-function-name-face)
	("let" . font-lock-type-face)
	("include\\|inputs+\\|outputs+\\|run\\|executor\\|eval" . font-lock-keyword-face)
	))
(define-derived-mode conseq-mode fundamental-mode "conseq"
  "Major mode for editing conseq files."
  (setq font-lock-defaults '(conseq-highlights)))

(provide 'conseq-mode)
