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

(provide 'init-org)
