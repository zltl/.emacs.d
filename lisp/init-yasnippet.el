
(use-package yasnippet
  :ensure t
  :hook ((go-mode . yas-minor-mode)
         (c++-mode . yas-minor-mode)
         (c-mode . yas-minor-mode)))

(provide 'init-yasnippet)
