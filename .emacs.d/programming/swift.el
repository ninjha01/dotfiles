;; Swift Mode Configuration

(use-package swift-mode 
  :ensure t 
  :mode ("\\.swift\\'" . swift-mode) 
  :hook (swift-mode . (lambda () 
                        (lsp))) 
  :config 
  (defun xcode-open-current-file() 
    (interactive) 
    (shell-command-to-string (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name)))) 
  
  (defun xcode-build() 
    (interactive) 
    (shell-command-to-string "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))
  
  (defun xcode-run() 
    (interactive) 
    (shell-command-to-string "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
  
  ;; Key bindings
  :bind (:map swift-mode-map 
              ("C-c C-c" . xcode-run) 
              ("C-c C-b" . xcode-build) 
              ("C-c C-o" . xcode-open-current-file)))

;; Swift formatter
(add-to-list 'apheleia-mode-alist '(swift-mode . swift-format))
(add-to-list 'apheleia-formatters '(swift-format "swift-format" (buffer-file-name)))

;; Swift linting and type checking
(use-package flycheck-swift 
  :ensure t 
  :after flycheck 
  :hook (flycheck-mode . flycheck-swift-setup) 
        (swift-mode . flycheck-mode) 
  :config 
  (setq flycheck-swift-sdk-path "/Users/nishantjha/Desktop/Xcode-beta.app/Contents/Developer/Platforms/XROS.platform/Developer/SDKs/XROS1.0.sdk")
  ;; Select the appropriate SDK version you use
  (setq flycheck-swift-target "arm64-apple-xros"))

;; LSP for Swift
(use-package lsp-sourcekit 
  :ensure t 
  :after lsp-mode 
  :config 
  (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))

(provide 'swift)