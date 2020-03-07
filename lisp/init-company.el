
;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(provide 'init-company)
