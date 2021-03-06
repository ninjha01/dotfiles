(setq org-package-list '(org-bullets org-plus-contrib ox-reveal))
(dolist (package org-package-list) 
  (unless (package-installed-p package) 
    (package-install package)))

(require 'org)
(require 'org-tempo)
(require 'org-capture)
(require 'ox-reveal)

(setq org-directory "~/Google Drive/org")
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-last-stored-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-S-<up>") nil)
(define-key org-mode-map (kbd "C-S-<down>") nil)

(setq my-notes-file "~/Google Drive/org/notes.org")
(setq my-tasks-file "~/Google Drive/org/work.org")
(setq my-journal-file "~/Google Drive/org/journal.org")


(setq org-default-notes-file my-notes-file)
(setq org-capture-templates '(("t" "Todo" entry (file+headline my-tasks-file "Tasks")
			       "* TODO %?\n  %i\n  %a") 
			      ("n" "Note" entry (file+datetree my-notes-file "Notes")
			       "** %U %^{Title} \n %? %a") 
			      ("j" "Journal" entry (file+datetree my-journal-file)
			       "* %?\nEntered on %U\n  %i\n  %a")))
(setq org-agenda-files (list my-notes-file my-tasks-file my-journal-file))
(add-hook 'org-mode-hook 
	  (lambda () 
	    (org-bullets-mode 1)
	    (auto-revert-mode 1)
	    ))
;;; Code blocks indent
(setq org-src-tab-acts-natively t)
;;; Code syntax highlight
(setq org-src-fontify-natively t)


;; plantuml
(setq org-plantuml-exec-mode 'plantuml)
(setq org-plantuml-executable-path (expand-file-name "/usr/local/bin/plantuml"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

;;; Org-babel languages
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t) 
							 (python . t)
							 (ocaml . t)
							 (sql . t)
							 (dot . t)
							 (plantuml . t)))
(setq org-log-done t)

;; org live refresh inline images
(defun org-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(with-eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-fix-inline-images))

(setq org-confirm-babel-evaluate nil)
(provide 'init-org)
