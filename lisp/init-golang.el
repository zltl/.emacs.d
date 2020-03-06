
(require 'init-load)

(load-vendor-path "vendor/go-mod")

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;;; lsp mode
;; first must install gopls and put it somwhere in your PATH
(use-package lsp-mode
             :ensure t
             :commands (lsp lsp-deferred)
             :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
             :ensure t
             :commands lsp-ui-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
             :ensure t
             :config
             ;; Optionally enable completion-as-you-type behavior.
             (setq company-idle-delay 0)
             (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
             :ensure t
             :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
             :ensure t
             :commands yas-minor-mode
             :hook (go-mode . yas-minor-mode))

;; go tags
;(use-package go-add-tags
;  :ensure t
;  :config (custom-set-variables
;           '(go-add-tags-style 'snake-case))
;  :hook (go-mode))

(provide 'init-golang)
