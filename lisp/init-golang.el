(use-package go-mode :ensure t)

(add-auto-mode 'go-mode "\\.go\\'")

;;; NO, too slow
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; go tags
(use-package go-add-tags
  :ensure t
  :config (custom-set-variables
           '(go-add-tags-style 'snake-case))
  :hook (go-mode))

(provide 'init-golang)
