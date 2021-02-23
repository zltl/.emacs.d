;;; init-org.el -- my orgmode config

(setq org-agenda-files (list "~/TODO/work-2021.org"
                             "~/TODO/me-2021.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELED(c@)")))

(provide 'init-org)
