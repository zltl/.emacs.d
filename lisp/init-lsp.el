
;;; lsp mode
(use-package lsp-mode
  :ensure t
  ;; :diminish nil
  :commands lsp
  :hook
  (go-mode . lsp)
  (c++-mode . lsp)
  (c-mode . lsp)
  (python-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  (setq lsp-enable-file-watchers t
        lsp-file-watch-threshold 100000)
  )

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :after (lsp-mode)
  :commands (lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ;; 查询符号定义：使用 LSP 来查询。通常是 M-.
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ;; 查询符号引用：使用 LSP 来查询。通常是 M-?
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ;; 该文件里的符号列表：类、方法、变量等。前提是语言服务支持本功能。
        ("C-c u" . lsp-ui-imenu))
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-delay 0.2
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-code-actions t
        lsp-modeline-code-actions-enable t
        lsp-signature-render-documentation t
        lsp-enable-symbol-highlighting t
        lsp-ui-sideline-update-mode t
        lsp-ui-doc-enable t
        lsp-eldoc-enable-hover t
        lsp-ui-imenu-auto-refresh t
        lsp-ui-imenu-refresh-delay 1
        )
  )

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
;; (load-vendor-path "vendor/company-lsp")
;; (require 'company-lsp)
;; (global-company-mode 1)
;; (push 'company-lsp company-backends)
;; (add-hook 'after-init-hook 'global-company-mode)
;; Disable client-side cache because the LSP server does a better job.
;; (setq company-transformers nil
;;       company-lsp-async t
;;       company-lsp-cache-candidates nil)

(use-package lsp-ivy
  :ensure t)
(provide 'init-lsp)
