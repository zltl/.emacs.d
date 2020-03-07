;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking) ;; Measure startup time

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; first load
(require 'init-load)
(require 'cl)

;; init package.el, and other helper function
;; Calls (package-initialize)
(require 'init-elpa)

;; better defaults
(require 'better-defaults)

;;; load configs for specific features and modes

(use-package diminish)
(maybe-require-package 'scratch)
(use-package command-log-mode
  :ensure t)

(require 'init-themes)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido-completing-read-plus
;; geting ido goodness everywhere else
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; find-file-in-project
(require 'find-file-in-project)

;; slime-style navigation for emacs lisp
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

(require 'init-company)
(require 'init-ivy)
(require 'init-treemacs)
(require 'init-cc)
(require 'init-golang)
(require 'init-lsp)
(require 'init-yasnippet)
(require 'init-git)
(require 'init-csv)

(use-package undo-tree
  :ensure t
  :commands (global-undo-tree-mode))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; init.el ends here

