
(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (put 'projectile-project-name 'safe-local-variable 'stringp)
  (put 'projectile-project-compilation-cmd 'safe-local-variable 'stringp)
  (put 'projectile-use-git-grep 'safe-local-variable 'booleanp)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  ;; seting the remote file exists cache to an hour, I'd rather things
  ;; error out weirdly than slow down all find-files!
  (setq projectile-file-exists-remote-cache-expire (* 60 60))
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)

  ;; this replaces the old `projectile-compile-project' to use the
  ;; project name in the compilation buffer. Let's me run all ze
  ;; compilations!
  (defun cm/projectile-compile-project (arg &optional dir)
         "Run project compilation command, using the project name

 Normally you'll be prompted for a compilation command, unless
 variable `compilation-read-command'.  You can force the prompt
 with a prefix ARG."
         (interactive "P")
         (let* ((project-root (if dir
                                  dir
                                (projectile-project-root)))
                (default-directory project-root)
                (default-cmd (projectile-compilation-command project-root))
                (compilation-cmd (if (or compilation-read-command arg)
                                     (projectile-read-command "Compile command: "
                                                              default-cmd)
                                   default-cmd)))
           (puthash project-root compilation-cmd projectile-compilation-cmd-map)
           (save-some-buffers (not compilation-ask-about-save)
                              (lambda ()
                                (projectile-project-buffer-p (current-buffer)
                                                             project-root)))
           (with-current-buffer
               (compilation-start compilation-cmd nil '(lambda (x) (concat "*compilation:" (projectile-project-name) "*")))
               (setq-local projectile-project-name (projectile-project-name)))))
  )


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

; (use-package treemacs-evil
;   :after treemacs evil
;   :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(provide 'init-treemacs)
