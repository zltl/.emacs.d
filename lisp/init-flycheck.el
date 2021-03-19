

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
;;  :config
 ;; (setq flycheck-check-syntax-automatically '(mode-enable save))
  )



(provide 'init-flycheck)
