

(use-package which-key
  :ensure t
  :diminish nil
  :config
  (which-key-mode)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))


(provide 'init-keybinds)
