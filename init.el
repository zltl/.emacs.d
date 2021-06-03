;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Code:

(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(setq my-cache-dir "~/.emacs.d/cache/")

;; Unix tools look for HOME, but this is normally not defined on Windows.
(when (and IS-WINDOWS (null (getenv-internal "HOME")))
  (setenv "HOME" (getenv "USERPROFILE"))
    (setq abbreviated-home-dir nil))

;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8-unix)       ; pretty
(setq locale-coding-system 'utf-8-unix)  ; please
;; The clipboard's on Windows could be in a wider encoding than utf-8 (likely
;; utf-16), so let Emacs/the OS decide what encoding to use there.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top
(setq default-buffer-file-coding-system 'utf-8-unix)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'subr-x)
(require 'cl-lib)
(require 'init-helper)


(require 'init-faster)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-load)
;; init package.el, and other helper function
;; Calls (package-initialize)
(require 'init-elpa)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

(use-package diminish
             :ensure t)
(use-package command-log-mode
  :ensure t)

(require 'init-editor)
(require 'init-project)
(require 'init-keybinds)
(require 'init-git)

;; neotree
(require 'init-dir)

;; smex
(use-package smex :ensure t)
(smex-initialize)

(use-package counsel :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))



;; ido-completing-read-plus
;; geting ido goodness everywhere else
(ido-mode 1)
(ido-everywhere 1)
(use-package ido-completing-read+ :ensure t)
(ido-ubiquitous-mode 1)

;; find-file-in-project
(use-package find-file-in-project :ensure t)

(setq create-lockfiles nil)
(setq make-backup-files nil)

(require 'init-themes)
(require 'init-org)

;; autopair
(use-package autopair :ensure t
  :diminish nil
  :config
  (autopair-global-mode) ;; enable autopair in all buffers  
  )

(require 'init-completion)
(require 'init-flycheck)

(use-package ctrlf :ensure t
  :config
  (ctrlf-mode +1))
(use-package swiper :ensure t
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

;; jump
(use-package avy :ensure t
  :bind (("C-'" . avy-goto-char-timer))
  :config
  (setq avy-background t
        avy-all-windows t
        avy-timeout-seconds 0.5))


(use-package multiple-cursors
  :ensure t
  )

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 4))

(require 'bazel)
(require 'init-cc)
(require 'init-golang)
(require 'init-lsp)

(require 'init-csv)
(require 'init-markdown)

(diminish 'abbrev-mode nil)

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  :init (global-undo-tree-mode))

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; navigate visually through the entire kill ring
(defun l/kill-ring-insert ()
  (interactive)
  (let ((result (completing-read
                 "Yank: "
                 (cl-delete-duplicates kill-ring :test #'equal))))
    (when (and result (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert result)))
(global-set-key (kbd "M-y") 'l/kill-ring-insert)

;; emacs -nw interact with the system clipboard across all kinds of things.
(use-package clipetty
  :bind ("C-c c" . clipetty-kill-ring-save))


;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))


(use-package fill-column-indicator
  :commands (fci-mode))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; init.el ends here

