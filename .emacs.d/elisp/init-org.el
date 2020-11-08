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
	    (org-bullets-mode 1)))
;;; Code blocks indent
(setq org-src-tab-acts-natively t)
;;; Code syntax highlight
(setq org-src-fontify-natively t)
;;; Org-babel shell
(org-babel-do-load-languages 'org-babel-load-languages '((shell . t) 
							 (python . t)))
(setq org-log-done t)

(provide 'init-org)
