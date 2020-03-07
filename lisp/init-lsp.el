
;;; lsp mode
(use-package lsp-mode
  :ensure t
  :hook
  (go-mode . lsp-deferred))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t)

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t)

(use-package lsp-ivy
  :ensure t)

(use-package lsp-treemacs
  :commands
  (lsp-treemacs-errors-list
   lsp-treemacs-symbols-list
   lsp-treemacs-references
   lsp-treemacs-implementations
   lsp-treemacs-call-hierarchy
   lsp-treemacs-deps-list
   lsp-metals-treeview)
  :config
  (lsp-metals-treeview-enable t))


(provide 'init-lsp)
