;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; optimization for lsp https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")
