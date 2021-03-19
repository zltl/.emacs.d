;;; init-org.el -- my orgmode config

(require 'org)

(setq org-src-fontify-natively t)

(setq org-agenda-files (file-expand-wildcards "~/TODO/*.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELED(c@)")))


(require 'org-loaddefs)
(require 'org-protocol)
(require 'org-mouse)
(require 'org-attach)

(setq org-default-notes-file (concat org-directory "~/TODO/capture.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/TODO/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/TODO/capture.org")
         "* %?\nEntered on %U\n  %i\n  %a")))


(provide 'init-org)
