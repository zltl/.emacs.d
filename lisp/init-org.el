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


(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/.emacs.d/org_brain/")
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
;; (use-package polymode
;;  :ensure t
;;  :config
;;  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

(use-package org-journal :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/TODO/"
        org-journal-date-format "%A, %d %B %Y")
  )

(use-package ob-async
  :ensure t
  )

(use-package org-cliplink :ensure t)
(use-package orgit :ensure t)
(use-package ox-clip :ensure t)
(use-package ob-go :ensure t)

(use-package gnuplot :ensure t)


(provide 'init-org)
