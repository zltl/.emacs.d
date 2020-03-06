
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun load-vendor-path (dir)
  "load-path add <dir>"
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(load-vendor-path "lisp")
(load-vendor-path "lib")

;; better defaults
(load-vendor-path "vendor/better-defaults")
(require 'better-defaults)

(require 'init-elpa)

;; spinner
(load-vendor-path "vendor/spinner")
(require 'spinner)

;; smex
(load-vendor-path "vendor/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido-completing-read-plus
;; geting ido goodness everywhere else
(load-vendor-path "vendor/ido-completing-read-plus")
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; find-file-in-project
(load-vendor-path "vendor/find-file-in-project")
(require 'find-file-in-project)

;; slime-style navigation for emacs lisp
(load-vendor-path "vendor/elisp-slime-nav")
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;; autopair
(load-vendor-path "vendor/autopair")
(require 'autopair)

(autopair-global-mode) ;; enable autopair in all buffers

(require 'init-golang)

(use-package undo-tree
  :ensure t
  :commands (global-undo-tree-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (go-add-tags undo-tree flycheck company-lsp lsp-ui lsp-mode yasnippet company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
