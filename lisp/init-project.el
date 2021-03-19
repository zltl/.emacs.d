
(use-package projectile
  :ensure t
  :init
  (setq projectile-cache-file (concat my-cache-dir "projectile.cache")
        ;; Auto-discovery is slow to do by default. Better to update the list
        ;; when you need to (`projectile-discover-projects-in-search-path').
        projectile-auto-discover nil
        projectile-enable-caching 1
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file (concat my-cache-dir "projectile.projects"))
  :config
  (projectile-mode +1)
  )

(provide 'init-project)
