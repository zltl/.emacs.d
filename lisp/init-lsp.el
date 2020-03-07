
;;; lsp mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (c++-mode . lsp-defered))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
