
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)
  (setq ivy-count-format ""))

(provide 'init-ivy)
