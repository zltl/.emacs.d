

(use-package yasnippet :ensure t
  :config
  (yas-global-mode 1))
(diminish 'yas-minor-mode nil)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init  . global-company-mode)
  :config
  (require 'counsel)
  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-show-numbers t
        company-minimum-prefix-length 1))

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)
  (setq ivy-count-format ""))

(provide 'init-completion)
