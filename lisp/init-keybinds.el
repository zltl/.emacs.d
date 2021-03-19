

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 10000)  
  (which-key-mode)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  )


(provide 'init-keybinds)
